// @ts-check
import { defineConfig } from 'astro/config';
import tailwind from '@astrojs/tailwind';

// Astro Local Runner - Optimized Configuration
// Zero-cost GitHub Pages deployment with local CI/CD workflows
export default defineConfig({
  // GitHub Pages deployment configuration for Emacs Documentation
  site: 'https://kairin.github.io',
  base: '/emacs-docs',

  // Integrations for modern web development
  integrations: [
    tailwind({
      // Enable base styles for better defaults
      applyBaseStyles: true,
    }),
  ],

  // Note: TypeScript strict mode is configured via tsconfig.json

  // Build optimization for GitHub Pages
  build: {
    // Inline stylesheets for better performance
    inlineStylesheets: 'auto',
    // Asset optimization
    assets: '_astro',
  },

  // Output to docs/ folder for GitHub Pages compatibility
  outDir: './docs',

  // Static site generation for GitHub Pages
  output: 'static',

  // Vite configuration for enhanced functionality
  vite: {
    plugins: [
      // Automatically create .nojekyll file for GitHub Pages
      {
        name: 'create-nojekyll',
        async writeBundle() {
          const fs = await import('fs');
          const path = await import('path');
          const nojekyllPath = path.join('./docs', '.nojekyll');

          // Ensure docs directory exists
          if (!fs.existsSync('./docs')) {
            console.warn('⚠️ WARNING: docs directory not found for .nojekyll creation');
            return;
          }

          // Create .nojekyll file (CRITICAL for GitHub Pages)
          fs.writeFileSync(nojekyllPath, '');
          console.log('✅ Created .nojekyll file for GitHub Pages');

          // Verify _astro directory exists (critical for asset loading)
          const astroDir = path.join('./docs', '_astro');
          if (fs.existsSync(astroDir)) {
            const files = fs.readdirSync(astroDir);
            console.log(`✅ _astro directory confirmed (${files.length} files)`);
          } else {
            console.warn('⚠️ WARNING: _astro directory not found - assets may not load');
          }

          // Create deployment info file
          const deployInfo = {
            buildTime: new Date().toISOString(),
            astroVersion: '5.x',
            deploymentTarget: 'GitHub Pages',
            cicdType: 'Local Runners'
          };
          fs.writeFileSync(
            path.join('./docs', 'deployment-info.json'), 
            JSON.stringify(deployInfo, null, 2)
          );
          console.log('📋 Created deployment info file');
        }
      }
    ],
    build: {
      // Performance optimization for smaller bundles
      rollupOptions: {
        output: {
          manualChunks: {
            // Keep vendor dependencies separate and small
            vendor: ['astro'],
          },
        },
      },
      // Minification for production
      minify: 'esbuild',
      // No source maps for smaller bundles
      sourcemap: false,
    },
    // Development optimizations
    server: {
      // Hot reload performance
      hmr: {
        overlay: false, // Reduce development overhead
      },
    },
  },

  // Note: SEO and performance optimization handled by build process

  // ✅ GitHub Pages compatible
  // ✅ Performance optimized
  // ✅ Local CI/CD ready
  // ✅ TypeScript strict mode (via tsconfig.json)
  // ✅ .nojekyll automatic creation
});
