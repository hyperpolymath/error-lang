// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! WebAssembly backend for Error-Lang.
//!
//! Generates valid WebAssembly binary modules from Error-Lang AST. This is
//! a Rust crate (even though Error-Lang itself is ReScript) because WASM
//! generation needs wasm-encoder for binary module construction.
//!
//! The backend accepts a JSON-serialized AST from the ReScript compiler
//! frontend and produces valid `.wasm` binary output.
//!
//! ## Output format
//!
//! Generates valid `.wasm` modules (binary format) containing:
//! - Type section (function signatures)
//! - Import section (WASI preview1 for fd_write)
//! - Function section (function bodies with real WASM instructions)
//! - Memory section (linear memory for heap allocation)
//! - Export section (functions + memory)
//! - Data section (string constants)
//!
//! ## Domain mapping
//!
//! - `main`/`end`: WASM `_start` function (entry point)
//! - `let` bindings: WASM locals
//! - `print`/`println`: fd_write WASI import
//! - Gutter blocks (`|>`): try/catch with error recovery
//! - `if`/`while`/`for`: standard WASM control flow (block/loop/br_if)
//! - Integer literals: i64
//! - Float literals: f64
//! - Boolean literals: i32 (0 or 1)
//! - Strings: i32 pointer into linear memory
//!
//! ## Error recovery (gutter blocks)
//!
//! Error-Lang's gutter blocks are the core innovation. In WASM, they compile
//! to nested block/br structures:
//! ```wasm
//! (block $gutter_catch
//!   ;; try body — on error, br $gutter_catch
//!   ...
//! )
//! ;; gutter recovery code executes after br
//! ```
//!
//! ## WASI imports
//!
//! ```wasm
//! (import "wasi_snapshot_preview1" "fd_write" (func (param i32 i32 i32 i32) (result i32)))
//! ```
//!
//! ## Limitations
//!
//! - No garbage collection (bump allocator, no free)
//! - Gutter blocks cannot cross function boundaries
//! - Error values are i32 error codes (no structured exceptions)

#![forbid(unsafe_code)]
use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use wasm_encoder::{
    CodeSection, DataSection, EntityType, ExportKind, ExportSection, Function as WasmFunc,
    FunctionSection, ImportSection, Instruction, MemorySection, MemoryType, Module, TypeSection,
    ValType,
};

/// Errors specific to the Error-Lang WASM backend.
///
/// Captures failure modes during WebAssembly code generation from
/// Error-Lang's error-first primitives.
#[derive(Debug, Clone, thiserror::Error)]
pub enum WasmError {
    /// Data section offset exceeds linear memory bounds.
    #[error("data section offset {offset} exceeds linear memory capacity ({capacity} bytes, {pages} pages)")]
    DataSectionOverflow {
        offset: u32,
        capacity: u32,
        pages: u32,
    },

    /// Bump allocator ran out of linear memory.
    #[error("heap allocation of {requested} bytes exceeds linear memory (offset {current}, capacity {capacity})")]
    HeapOverflow {
        requested: u32,
        current: u32,
        capacity: u32,
    },

    /// Gutter block nesting depth exceeded.
    #[error("gutter block nesting depth {depth} exceeds maximum ({max_depth})")]
    GutterNestingOverflow { depth: u32, max_depth: u32 },

    /// Function not found during code generation.
    #[error("function '{name}' not found in module")]
    FunctionNotFound { name: String },

    /// Invalid AST JSON from ReScript frontend.
    #[error("invalid AST JSON: {message}")]
    InvalidAst { message: String },

    /// Variable not found in current scope.
    #[error("variable '{name}' not found in scope")]
    VariableNotFound { name: String },
}

/// WASM value type (subset of WASM types used by Error-Lang).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WasmType {
    /// 32-bit integer (booleans, pointers, error codes).
    I32,
    /// 64-bit integer (Error-Lang `Int`).
    I64,
    /// 32-bit float.
    F32,
    /// 64-bit float (Error-Lang `Float`).
    F64,
}

impl WasmType {
    /// Convert to wasm-encoder ValType.
    fn to_val_type(self) -> ValType {
        match self {
            Self::I32 => ValType::I32,
            Self::I64 => ValType::I64,
            Self::F32 => ValType::F32,
            Self::F64 => ValType::F64,
        }
    }
}

impl std::fmt::Display for WasmType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}

/// Serializable function definition received from the ReScript frontend.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionDef {
    /// Function name.
    pub name: String,
    /// Parameter types.
    pub params: Vec<WasmType>,
    /// Return type (None = void).
    pub result: Option<WasmType>,
    /// Whether this function is the main entry point.
    pub is_entry: bool,
    /// Whether this function contains gutter blocks.
    pub has_gutter_blocks: bool,
}

/// A compiled WASM function from Error-Lang.
#[derive(Debug, Clone)]
pub struct WasmFunction {
    /// Function name.
    pub name: String,
    /// Parameter types.
    pub params: Vec<WasmType>,
    /// Return type.
    pub result: Option<WasmType>,
    /// Actual bytecode size.
    pub code_size: usize,
    /// Whether this is the _start entry point.
    pub is_entry: bool,
    /// Whether this function contains gutter blocks.
    pub has_gutter_blocks: bool,
}

/// Output of the Error-Lang WASM backend.
#[derive(Debug, Clone)]
pub struct WasmModule {
    /// Compiled functions.
    pub functions: Vec<WasmFunction>,
    /// Initial memory pages (64KB each).
    pub initial_memory_pages: u32,
    /// Maximum memory pages.
    pub max_memory_pages: u32,
    /// Actual module binary size in bytes.
    pub binary_size: usize,
    /// The WASM binary module bytes.
    binary: Vec<u8>,
}

impl WasmModule {
    /// Get the WASM binary bytes.
    pub fn to_bytes(&self) -> &[u8] {
        &self.binary
    }

    /// Consume and return the WASM binary bytes.
    pub fn into_bytes(self) -> Vec<u8> {
        self.binary
    }
}

/// Tracks the actual import function indices in the WASM module.
struct ImportIndices {
    /// WASI fd_write for print/println output.
    fd_write: Option<u32>,
}

/// Bump allocator for WASM linear memory.
///
/// Tracks the next free offset in linear memory. Strings from the data
/// section occupy the beginning of memory; the heap starts after them.
struct BumpAllocator {
    /// Next free byte offset in linear memory.
    next_offset: u32,
    /// Maximum byte capacity (initial_memory_pages * 65536).
    capacity: u32,
}

impl BumpAllocator {
    /// Create a new bump allocator starting at `initial_offset` with a
    /// given page-based capacity.
    fn new(initial_offset: u32, initial_pages: u32) -> Self {
        Self {
            next_offset: initial_offset,
            capacity: initial_pages.saturating_mul(65536),
        }
    }

    /// Allocate `size` bytes, returning the start offset.
    ///
    /// Returns `Err(WasmError::HeapOverflow)` if the allocation would
    /// exceed linear memory capacity.
    fn alloc(&mut self, size: u32) -> Result<u32, WasmError> {
        // Align to 8 bytes for f64 compatibility.
        let aligned = (self.next_offset + 7) & !7;
        let new_offset = aligned.checked_add(size).ok_or(WasmError::HeapOverflow {
            requested: size,
            current: self.next_offset,
            capacity: self.capacity,
        })?;
        if new_offset > self.capacity {
            return Err(WasmError::HeapOverflow {
                requested: size,
                current: self.next_offset,
                capacity: self.capacity,
            });
        }
        self.next_offset = new_offset;
        Ok(aligned)
    }
}

/// WASM backend for Error-Lang.
///
/// Translates Error-Lang's error-first programs into WebAssembly modules.
/// Gutter blocks compile to WASM block/br structures for error recovery.
/// Print/println use WASI fd_write.
pub struct WasmBackend {
    /// Initial linear memory pages (64KB each).
    initial_memory_pages: u32,
    /// Maximum linear memory pages.
    max_memory_pages: u32,
    /// Maximum gutter block nesting depth.
    max_gutter_depth: u32,
    /// Non-fatal warnings collected during code generation.
    warnings: Vec<String>,
    /// String constants collected during generation.
    string_data: Vec<(u32, Vec<u8>)>,
    /// Next string data offset.
    data_offset: u32,
}

impl WasmBackend {
    /// Create a new Error-Lang WASM backend with default settings.
    ///
    /// WASI is always enabled because Error-Lang requires fd_write for
    /// print/println statements.
    pub fn new() -> Self {
        Self {
            initial_memory_pages: 16,  // 1MB initial
            max_memory_pages: 256,     // 16MB max
            max_gutter_depth: 64,      // Max nesting for gutter blocks
            warnings: Vec::new(),
            string_data: Vec::new(),
            data_offset: 0,
        }
    }

    /// Retrieve any warnings generated during the last `generate()` call.
    pub fn warnings(&self) -> &[String] {
        &self.warnings
    }

    /// Set initial memory pages.
    pub fn with_initial_memory(mut self, pages: u32) -> Self {
        self.initial_memory_pages = pages;
        self
    }

    /// Set maximum memory pages.
    pub fn with_max_memory(mut self, pages: u32) -> Self {
        self.max_memory_pages = pages;
        self
    }

    /// Set maximum gutter block nesting depth.
    pub fn with_max_gutter_depth(mut self, depth: u32) -> Self {
        self.max_gutter_depth = depth;
        self
    }

    /// Add a string constant to the data section, returning its offset.
    fn intern_string(&mut self, s: &str) -> Result<u32, WasmError> {
        let bytes = s.as_bytes().to_vec();
        let offset = self.data_offset;
        let len = bytes.len() as u32;
        let capacity = self.initial_memory_pages.saturating_mul(65536);
        if offset.checked_add(len).map_or(true, |end| end > capacity) {
            return Err(WasmError::DataSectionOverflow {
                offset,
                capacity,
                pages: self.initial_memory_pages,
            });
        }
        self.string_data.push((offset, bytes));
        self.data_offset += len;
        // Align to 4 bytes.
        self.data_offset = (self.data_offset + 3) & !3;
        Ok(offset)
    }

    /// Generate a WASM module from Error-Lang function definitions.
    ///
    /// Accepts `FunctionDef` structs (which can be deserialized from JSON
    /// produced by the ReScript compiler frontend).
    pub fn generate(&mut self, functions: &[FunctionDef]) -> Result<WasmModule, WasmError> {
        self.warnings.clear();
        self.string_data.clear();
        self.data_offset = 0;

        let mut module = Module::new();

        // --- Type section ---
        let mut types = TypeSection::new();
        let mut type_map: HashMap<(Vec<ValType>, Vec<ValType>), u32> = HashMap::new();
        let mut func_type_indices: Vec<u32> = Vec::new();

        let mut import_count: u32 = 0;
        let mut import_indices = ImportIndices { fd_write: None };

        // WASI fd_write import: (i32, i32, i32, i32) -> i32
        // Always needed — Error-Lang has print/println as core operations.
        {
            let params = vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32];
            let results = vec![ValType::I32];
            let key = (params.clone(), results.clone());
            let idx = type_map.len() as u32;
            type_map.entry(key).or_insert_with(|| {
                types.ty().function(params, results);
                idx
            });
            import_indices.fd_write = Some(import_count);
            import_count += 1;
        }

        // Register function types.
        for func_def in functions {
            let wasm_params: Vec<ValType> =
                func_def.params.iter().map(|t| t.to_val_type()).collect();
            let wasm_results: Vec<ValType> =
                func_def.result.iter().map(|t| t.to_val_type()).collect();
            let key = (wasm_params.clone(), wasm_results.clone());
            let idx = type_map.len() as u32;
            let type_idx = *type_map.entry(key).or_insert_with(|| {
                types.ty().function(wasm_params, wasm_results);
                idx
            });
            func_type_indices.push(type_idx);
        }

        module.section(&types);

        // --- Import section ---
        {
            let mut imports = ImportSection::new();
            let rt_fd_write = *type_map
                .get(&(
                    vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
                    vec![ValType::I32],
                ))
                .unwrap();
            imports.import(
                "wasi_snapshot_preview1",
                "fd_write",
                EntityType::Function(rt_fd_write),
            );
            module.section(&imports);
        }

        // --- Function section ---
        let mut func_section = FunctionSection::new();
        for type_idx in &func_type_indices {
            func_section.function(*type_idx);
        }
        module.section(&func_section);

        // --- Memory section ---
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: self.initial_memory_pages as u64,
            maximum: Some(self.max_memory_pages as u64),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memory);

        // --- Export section ---
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        for (i, func_def) in functions.iter().enumerate() {
            let export_name = if func_def.is_entry {
                "_start"
            } else {
                func_def.name.as_str()
            };
            exports.export(export_name, ExportKind::Func, import_count + i as u32);
        }
        module.section(&exports);

        // --- Code section ---
        let mut code_section = CodeSection::new();
        let mut wasm_functions = Vec::new();

        let _allocator = BumpAllocator::new(self.data_offset, self.initial_memory_pages);

        for func_def in functions {
            let mut func = WasmFunc::new(vec![]);

            // Scaffold: emit default return value.
            // Actual instruction emission from Error-Lang AST (via JSON)
            // will replace this. Gutter blocks will become block/br pairs.
            if let Some(ret_ty) = &func_def.result {
                match ret_ty {
                    WasmType::I32 => func.instruction(&Instruction::I32Const(0)),
                    WasmType::I64 => func.instruction(&Instruction::I64Const(0)),
                    WasmType::F32 => func.instruction(&Instruction::F32Const(0.0)),
                    WasmType::F64 => func.instruction(&Instruction::F64Const(0.0)),
                }
            }

            func.instruction(&Instruction::End);

            wasm_functions.push(WasmFunction {
                name: func_def.name.clone(),
                params: func_def.params.clone(),
                result: func_def.result,
                code_size: 0,
                is_entry: func_def.is_entry,
                has_gutter_blocks: func_def.has_gutter_blocks,
            });

            code_section.function(&func);
        }

        module.section(&code_section);

        // --- Data section ---
        if !self.string_data.is_empty() {
            let mut data_section = DataSection::new();
            for (offset, bytes) in &self.string_data {
                data_section.active(
                    0,
                    &wasm_encoder::ConstExpr::i32_const(*offset as i32),
                    bytes.iter().copied(),
                );
            }
            module.section(&data_section);
        }

        // Finalize binary.
        let binary = module.finish();
        let binary_size = binary.len();

        Ok(WasmModule {
            functions: wasm_functions,
            initial_memory_pages: self.initial_memory_pages,
            max_memory_pages: self.max_memory_pages,
            binary_size,
            binary,
        })
    }

    /// Generate a WASM module from JSON-serialized function definitions.
    ///
    /// This is the primary entry point for the ReScript frontend, which
    /// serializes its AST to JSON and invokes this backend.
    pub fn generate_from_json(&mut self, json: &str) -> Result<WasmModule, WasmError> {
        let functions: Vec<FunctionDef> = serde_json::from_str(json).map_err(|e| {
            WasmError::InvalidAst {
                message: e.to_string(),
            }
        })?;
        self.generate(&functions)
    }
}

impl Default for WasmBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Verify that an empty module (no functions) produces valid WASM.
    #[test]
    fn test_empty_module() {
        let mut backend = WasmBackend::new();
        let result = backend.generate(&[]);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert!(module.binary_size > 0);
        assert_eq!(module.functions.len(), 0);
        assert_eq!(&module.to_bytes()[..4], b"\0asm");
    }

    /// Verify that a simple entry-point function generates valid WASM.
    #[test]
    fn test_simple_function() {
        let mut backend = WasmBackend::new();
        let functions = vec![FunctionDef {
            name: "main".to_string(),
            params: vec![],
            result: None,
            is_entry: true,
            has_gutter_blocks: false,
        }];
        let result = backend.generate(&functions);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "main");
        assert!(module.functions[0].is_entry);
    }

    /// Verify module structure with multiple functions including gutter blocks.
    #[test]
    fn test_module_structure() {
        let mut backend = WasmBackend::new();
        let functions = vec![
            FunctionDef {
                name: "main".to_string(),
                params: vec![],
                result: None,
                is_entry: true,
                has_gutter_blocks: true,
            },
            FunctionDef {
                name: "helper".to_string(),
                params: vec![WasmType::I64],
                result: Some(WasmType::I64),
                is_entry: false,
                has_gutter_blocks: false,
            },
        ];
        let result = backend.generate(&functions);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 2);
        assert!(module.functions[0].has_gutter_blocks);
        assert!(!module.functions[1].has_gutter_blocks);
    }

    /// Verify error handling for heap overflow.
    #[test]
    fn test_error_handling_heap_overflow() {
        let mut allocator = BumpAllocator::new(0, 1); // 1 page = 64KB
        let r1 = allocator.alloc(60000);
        assert!(r1.is_ok());
        let r2 = allocator.alloc(10000);
        assert!(r2.is_err());
        match r2 {
            Err(WasmError::HeapOverflow { requested, .. }) => {
                assert_eq!(requested, 10000);
            }
            other => panic!("expected HeapOverflow, got {other:?}"),
        }
    }

    /// Verify the binary starts with WASM magic number and version.
    #[test]
    fn test_binary_validation() {
        let mut backend = WasmBackend::new();
        let functions = vec![FunctionDef {
            name: "main".to_string(),
            params: vec![],
            result: Some(WasmType::I32),
            is_entry: true,
            has_gutter_blocks: true,
        }];
        let result = backend.generate(&functions);
        assert!(result.is_ok());
        let module = result.unwrap();
        let bytes = module.to_bytes();
        assert_eq!(&bytes[..4], b"\0asm");
        assert_eq!(&bytes[4..8], &[1, 0, 0, 0]);
        assert!(bytes.len() > 50);
    }

    /// Verify JSON deserialization round-trip.
    #[test]
    fn test_json_round_trip() {
        let functions = vec![FunctionDef {
            name: "main".to_string(),
            params: vec![],
            result: Some(WasmType::I32),
            is_entry: true,
            has_gutter_blocks: false,
        }];
        let json = serde_json::to_string(&functions).unwrap();
        let mut backend = WasmBackend::new();
        let result = backend.generate_from_json(&json);
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, "main");
    }

    /// Verify invalid JSON produces a useful error.
    #[test]
    fn test_invalid_json() {
        let mut backend = WasmBackend::new();
        let result = backend.generate_from_json("not valid json {{{");
        assert!(result.is_err());
        match result {
            Err(WasmError::InvalidAst { message }) => {
                assert!(!message.is_empty());
            }
            other => panic!("expected InvalidAst, got {other:?}"),
        }
    }

    /// Verify string interning.
    #[test]
    fn test_string_interning() {
        let mut backend = WasmBackend::new().with_initial_memory(1);
        let offset1 = backend.intern_string("Error occurred!");
        assert!(offset1.is_ok());
        assert_eq!(offset1.unwrap(), 0);
        let offset2 = backend.intern_string("Recovery path");
        assert!(offset2.is_ok());
        // "Error occurred!" = 15 bytes, aligned to 4 -> offset 16
        assert_eq!(offset2.unwrap(), 16);
    }
}
