// SPDX-License-Identifier: MPL-2.0
// vite.config.js - Build configuration
// Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

import { defineConfig } from 'vite'

export default defineConfig({
  root: '.',
  publicDir: 'public',
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    sourcemap: true,
    // Ensure Monaco workers are bundled correctly
    rollupOptions: {
      output: {
        manualChunks: undefined,
      },
    },
  },
  server: {
    port: 3000,
    open: true,
  },
  // Monaco is loaded via CDN AMD loader, not bundled by Vite
  // Vite only handles the ReScript-compiled application code
  optimizeDeps: {
    exclude: ['monaco-editor'],
  },
})
