import { defineConfig } from 'tsup';

export default defineConfig({
  entry: ['src/index/index.ts', 'src/cli/cli.ts', 'src/playground.ts'],
    format: ['cjs', 'esm'], // Build for both CommonJS and ES Modules
  dts: true, // Generate .d.ts type definitions
  splitting: false, // Disable code splitting (optional)
  clean: true, // Clean the output directory before building
  outDir: 'dist', // Output directory
  target: 'es6', // Target ES6 for compatibility
});
