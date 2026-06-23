#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# install-affinescript-toolchain.sh — build & install the AffineScript compiler
# (hyperpolymath/affinescript, the OCaml/dune compiler) so error-lang's `.affine`
# sources — which replace the legacy ReScript per the Hyperpolymath language
# policy — can be typechecked and compiled.
#
# Why build from distro OCaml packages instead of opam: some CI/network policies
# block opam.ocaml.org. Every dependency below is available from the Debian/Ubuntu
# archive at a version satisfying affinescript's dune-project constraints
# (notably ocaml-dune 3.14 == `(lang dune 3.14)`).
#
# Network note: this clones github.com/hyperpolymath/affinescript directly; run it
# where GitHub is reachable (a normal CI runner or dev box).
set -euo pipefail

AFFINE_REPO="${AFFINE_REPO:-https://github.com/hyperpolymath/affinescript}"
AFFINE_SRC="${AFFINE_SRC:-${TMPDIR:-/tmp}/affinescript}"
PREFIX="${PREFIX:-/usr/local}"
SUDO="$(command -v sudo || true)"

# 1. OCaml toolchain + AffineScript build dependencies.
$SUDO apt-get update
$SUDO apt-get install -y \
  ocaml-dune menhir libmenhir-ocaml-dev libsedlex-ocaml-dev \
  libppx-deriving-ocaml-dev libppx-sexp-conv-ocaml-dev libsexplib0-ocaml-dev \
  libfmt-ocaml-dev libcmdliner-ocaml-dev libyojson-ocaml-dev \
  libppxlib-ocaml-dev libjs-of-ocaml-dev

# 2. Fetch + build the compiler binary.
[ -d "$AFFINE_SRC/.git" ] || git clone --depth 1 "$AFFINE_REPO" "$AFFINE_SRC"
( cd "$AFFINE_SRC" && dune build bin/main.exe )

# 3. Install the binary + stdlib. The module loader discovers the stdlib at
#    <binary_dir>/../share/affinescript/stdlib, so this needs no env var.
$SUDO install -m755 "$AFFINE_SRC/_build/default/bin/main.exe" "$PREFIX/bin/affinescript"
$SUDO mkdir -p "$PREFIX/share/affinescript"
$SUDO rm -rf "$PREFIX/share/affinescript/stdlib"
$SUDO cp -r "$AFFINE_SRC/stdlib" "$PREFIX/share/affinescript/stdlib"

echo "Installed: $(command -v affinescript)"
affinescript check "$AFFINE_SRC/examples/hello.affine" || true
echo "AffineScript toolchain installed under $PREFIX."
