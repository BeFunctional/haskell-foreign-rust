// Forward-declare the Rust-exported function
void haskell_ffi_external_free(void* vec);

// Wrapper around the Rust function that takes an additional (unused) argument,
// which makes it match the Haskell `FinalizerEnvPtr` type. The wrapper also
// avoids linker errors when the Rust library is not available (of course,
// the Rust library must be linked into the final application).
void haskell_ffi_external_free_env(void* vec, void* ptr) {
    haskell_ffi_external_free(vec);
}