// SPDX-License-Identifier: MPL-2.0
// Deno Runtime Bindings for ReScript
// Adapted from hyperpolymath/rescript-wasm-runtime

// Console bindings
@scope("console") @val external log: 'a => unit = "log"
@scope("console") @val external error: 'a => unit = "error"
@scope("console") @val external warn: 'a => unit = "warn"

// Environment bindings
module Env = {
  @scope("Deno") @val external get: string => option<string> = "env.get"
  @scope("Deno") @val external set: (string, string) => unit = "env.set"
}

// File system bindings
module Fs = {
  @scope("Deno") @val external readTextFile: string => promise<string> = "readTextFile"
  @scope("Deno") @val external writeTextFile: (string, string) => promise<unit> = "writeTextFile"
  @scope("Deno") @val external readFile: string => promise<Js.Typed_array.Uint8Array.t> = "readFile"
  @scope("Deno") @val external writeFile: (string, Js.Typed_array.Uint8Array.t) => promise<unit> = "writeFile"
}

// Date/Time
@val external now: unit => float = "Date.now"

// URL utilities
module Url = {
  @new external make: string => 'url = "URL"
  @get external pathname: 'url => string = "pathname"
  @get external search: 'url => string = "search"
  @get external searchParams: 'url => 'searchParams = "searchParams"
  @send external get: ('searchParams, string) => option<string> = "get"
}

// Process exit
@scope("Deno") @val external exit: int => unit = "exit"

// Args
@scope("Deno") @val external args: array<string> = "args"
