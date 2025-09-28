# Emacs Documentation - AI Agent Instructions

> 🤖 **CRITICAL**: This file (AGENTS.md) is the PRIMARY instruction document for ALL AI assistants (Claude, Gemini, ChatGPT, etc.) working on this repository. ALL requirements in this file are NON-NEGOTIABLE and MUST be followed at ALL times.

> 📝 **NOTE**: CLAUDE.md, GEMINI.md, and other AI-specific files are symlinks to this AGENTS.md file to ensure consistent instructions across all AI platforms.

## 🎯 Project Overview

**Emacs Documentation** is a comprehensive documentation website for Emacs built with modern web technologies and local CI/CD workflows. It provides zero-cost GitHub Pages deployment with performance optimization and accessibility features for the Emacs community.

**Repository**: https://github.com/kairin/emacs-docs
**Live Site**: https://kairin.github.io/emacs-docs/
**Integration**: Ready for [agents.md](https://agents.md/) workflow

## ⚡ NON-NEGOTIABLE REQUIREMENTS

### 🚨 CRITICAL: Zero-Cost Development & Deployment (MANDATORY)

#### Cost-Free Operations (NON-NEGOTIABLE)
- **NO CLOUD CI/CD SERVICES**: All CI/CD runs locally first
- **NO SUBSCRIPTION FEES**: No external services that incur charges
- **GITHUB PAGES ONLY**: Free static site hosting from docs/ folder
- **LOCAL RUNNERS FIRST**: All workflows MUST run locally before GitHub deployment
- **COST MONITORING**: Zero GitHub Actions usage unless absolutely necessary

#### Local CI/CD Infrastructure (MANDATORY)
```bash
# CRITICAL: All development workflows are local-first
make setup      # Complete environment setup
make ci         # Full local CI pipeline (check + test + build + verify)
make deploy     # Prepare deployment (build + verify GitHub Pages compatibility)
make dev        # Enhanced development server with hot reload
```

### 🚨 CRITICAL: Branch Management & Git Strategy (MANDATORY)

#### Branch Preservation (MANDATORY)
- **NEVER DELETE BRANCHES** without explicit user permission
- **ALL BRANCHES** contain valuable development history
- **NO** automatic cleanup with `git branch -d`
- **YES** to automatic merge to main branch, preserving dedicated branch

#### Branch Naming (MANDATORY SCHEMA)
**Format**: `YYYYMMDD-HHMMSS-type-short-description`

Examples:
- `20250928-143000-feat-astro-local-ci`
- `20250928-143515-fix-github-pages-config`
- `20250928-144030-docs-local-runner-setup`

#### GitHub Safety Strategy (MANDATORY)
```bash
# MANDATORY: Every commit must use this workflow with proper attribution
DATETIME=$(date +"%Y%m%d-%H%M%S")
BRANCH_NAME="${DATETIME}-feat-description"
git checkout -b "$BRANCH_NAME"

# Local CI MUST pass before pushing
make ci  # CRITICAL: Always run local CI first

git add .
git commit -m "Descriptive commit message

🤖 Generated with [AI Assistant Name] (Claude Code/GitHub Copilot CLI/Gemini CLI)
Co-Authored-By: [Assistant] <noreply@[platform].com>"
git push -u origin "$BRANCH_NAME"
git checkout main
git merge "$BRANCH_NAME" --no-ff
git push origin main
# NEVER: git branch -d "$BRANCH_NAME"
```

### 🚨 CRITICAL: GitHub Pages Deployment (MANDATORY)

#### Website Build Requirements (NON-NEGOTIABLE)
> ⚠️ **CRITICAL**: Failure to follow these requirements will cause 404 errors on GitHub Pages deployment

- **NEVER DELETE** built website files from `docs/` directory without rebuilding
- **ALWAYS BUILD** before committing any changes that affect the website
- **MANDATORY VERIFICATION** that GitHub Pages deployment remains functional
- **ZERO TOLERANCE** for commits that break the live website

#### Build Process (MANDATORY WORKFLOW)
```bash
# CRITICAL: ALWAYS run before any commit that might affect the website
npm run build                     # Build Astro website to docs/
test -f docs/index.html || exit 1     # Verify main page exists
test -d docs/_astro || exit 1          # Verify assets directory exists
test -f docs/.nojekyll || exit 1       # Verify GitHub Pages config exists

# MANDATORY: Verify build outputs before committing
ls -la docs/                      # Must show: index.html, _astro/, .nojekyll
git add docs/                     # Stage ALL built files
git commit -m "Deploy: $(date)"   # Commit with descriptive message
git push origin main              # Deploy to GitHub Pages
```

#### Deployment Validation (MANDATORY)
- **BEFORE**: Always check that the live site is accessible
- **AFTER**: Verify website loads within 5 minutes of pushing to main
- **ERROR RECOVERY**: If 404 occurs, immediately run `make ci && git add docs/ && git commit && git push`

#### Astro Configuration Requirements (NON-NEGOTIABLE)
```javascript
// astro.config.mjs MANDATORY settings
export default defineConfig({
  site: 'https://YOUR_USERNAME.github.io',  // REQUIRED for GitHub Pages
  base: '/astro-local-runner',               // REQUIRED for correct routing
  outDir: './docs',                          // REQUIRED for GitHub Pages source
  output: 'static',                          // REQUIRED for static site generation
  
  vite: {
    plugins: [
      // CRITICAL: Auto-create .nojekyll file
      {
        name: 'create-nojekyll',
        async writeBundle() {
          // Implementation ensures .nojekyll exists for GitHub Pages
        }
      }
    ]
  }
});
```

### 🚨 CRITICAL: Modern Web Development Stack (MANDATORY)

#### Technology Stack (NON-NEGOTIABLE)
- **Astro.build 5.x+**: Modern static site generator
- **TypeScript**: Strict mode enabled for type safety
- **Tailwind CSS**: Utility-first CSS framework
- **Node.js 18+**: Modern JavaScript runtime
- **Performance Target**: Lighthouse scores 95+ on all metrics

#### Package.json Scripts (MANDATORY)
```json
{
  "scripts": {
    "dev": "astro dev",
    "build": "astro check && astro build",
    "preview": "astro preview",
    "check": "astro check",
    "clean": "rm -rf ./docs/* ./dist ./.astro",
    "ci": "npm run check && npm run build"
  }
}
```

### 🚨 CRITICAL: Local CI/CD Infrastructure (MANDATORY)

#### Local Runners (NON-NEGOTIABLE)
All CI/CD operations MUST be available locally before GitHub integration:

```bash
# Local infrastructure structure (REQUIRED)
local-infra/
├── runners/
│   ├── deploy.sh                 # Enhanced deployment with validation
│   ├── gh-pages-setup.sh         # GitHub repository and Pages setup
│   ├── performance-check.sh      # Build optimization validation
│   └── development-server.sh     # Enhanced dev server with CI integration
├── logs/                         # Automated logging for all operations
└── config/                       # Local configuration files
```

#### Makefile Commands (MANDATORY)
```bash
# Primary workflow commands (REQUIRED)
make setup           # Complete environment setup
make ci              # Full local CI pipeline
make deploy          # Build and prepare for GitHub Pages
make dev             # Enhanced development server
make clean           # Clean build artifacts
make status          # System status and health check

# Advanced workflow commands
make branch          # Create timestamped branch
make commit          # CI + commit with validation
make push            # Push to remote with safety checks
make workflow        # Complete: branch + ci + commit + push
```

#### Performance Requirements (NON-NEGOTIABLE)
- **Build Time**: <30 seconds for complete build
- **Hot Reload**: <1 second for development changes
- **Bundle Size**: JavaScript <100KB, CSS <50KB
- **Local CI**: Complete pipeline <60 seconds
- **Lighthouse**: 95+ scores on Performance, Accessibility, Best Practices, SEO

### 🚨 CRITICAL: Quality Assurance (MANDATORY)

#### Code Quality (NON-NEGOTIABLE)
```bash
# MANDATORY: Quality checks before every commit
npm run check        # TypeScript and Astro validation
npm run build        # Production build verification
make verify          # GitHub Pages compatibility check
```

#### File Structure (MANDATORY)
```
astro-local-runner/
├── src/
│   ├── layouts/     # Page layouts
│   ├── pages/       # Site pages
│   ├── components/  # Reusable components
│   └── styles/      # Global styles
├── public/          # Static assets
├── docs/            # Built website (GitHub Pages source)
├── local-infra/     # Local CI/CD infrastructure
│   ├── runners/     # Automated workflow scripts
│   ├── logs/        # Operation logs
│   └── config/      # Local configuration
├── .github/         # GitHub Actions (minimal usage)
├── astro.config.mjs # Astro configuration
├── tailwind.config.mjs # Tailwind configuration
├── package.json     # Node.js dependencies and scripts
├── Makefile         # Local CI/CD automation
├── AGENTS.md        # Primary AI instruction file (this file)
├── CLAUDE.md        # Symlink to AGENTS.md
├── GEMINI.md        # Symlink to AGENTS.md
└── README.md        # Project documentation
```

## 🏗️ Development Standards

### Local Development Workflow (MANDATORY)
```bash
# 1. Setup (one-time)
make setup

# 2. Create feature branch
make branch
# Enter: feat, add-new-component

# 3. Development cycle
make dev              # Start development server
# Edit files in src/
# Changes auto-reload in browser

# 4. Pre-commit validation
make ci               # Local CI pipeline
# ✅ TypeScript checks pass
# ✅ Build successful
# ✅ GitHub Pages compatibility verified

# 5. Commit and deploy
make commit           # Automated commit with CI validation
# Enter commit message
make push             # Push to GitHub with auto-deployment
```

### Performance Optimization (MANDATORY)
- **Asset Optimization**: Automatic image compression and code splitting
- **Bundle Analysis**: Monitor JavaScript and CSS bundle sizes
- **Caching Strategy**: Proper cache headers for static assets
- **SEO Optimization**: Meta tags, sitemap, robots.txt
- **Accessibility**: WCAG 2.1 AA compliance

### Error Handling (MANDATORY)
```bash
# Local error recovery workflows
make clean            # Clean all build artifacts
make emergency-reset  # Reset to last known good state
make status           # Comprehensive system health check
```

## 🚨 ABSOLUTE PROHIBITIONS

### DO NOT
- Delete branches without explicit user permission
- **Delete built website files from `docs/` directory without rebuilding**
- **Commit changes that break GitHub Pages deployment**
- **Push to main without running local CI pipeline (`make ci`)**
- Use cloud CI/CD services that incur costs
- Bypass TypeScript strict mode requirements
- Skip performance validation checks
- Ignore build verification steps
- Remove or modify `.nojekyll` file
- Change Astro configuration without understanding GitHub Pages impact

### DO NOT BYPASS
- **Local CI pipeline before any commit (`make ci`)**
- **GitHub Pages build and deployment verification**
- **Website functionality validation before pushing**
- Branch preservation requirements
- TypeScript strict mode enforcement
- Performance optimization requirements
- Build artifact verification
- Deployment compatibility checks

## ✅ MANDATORY ACTIONS

### Before Every Commit
1. **Local CI Pipeline**: Run `make ci` and ensure all checks pass
2. **Website Build**: Verify `docs/` contains complete built website
3. **GitHub Pages Test**: Confirm deployment compatibility
4. **Performance Check**: Validate bundle sizes and optimization
5. **Branch Creation**: Use datetime-based branch naming
6. **Type Validation**: Ensure TypeScript strict mode compliance
7. **Asset Verification**: Confirm all assets are properly optimized
8. **Documentation**: Update relevant docs if adding features

### Quality Gates
- **Local CI Pipeline passes without errors**
- **All required files exist in `docs/`: index.html, _astro/, .nojekyll**
- **TypeScript strict mode validation passes**
- **Build size within performance targets**
- **GitHub Pages deployment compatibility confirmed**
- **No broken links or missing assets**
- **Lighthouse scores meet 95+ target**

## 🎯 Success Criteria

### Functionality Metrics
- **Setup Time**: <5 minutes for complete environment setup
- **Build Performance**: <30 seconds for full production build
- **Development Speed**: <1 second hot reload for changes
- **Deployment Success**: >99.9% successful GitHub Pages deployments
- **Local CI Reliability**: 100% consistency between local and remote builds

### Code Quality Metrics
- **Type Safety**: 100% TypeScript coverage with strict mode
- **Performance**: Lighthouse scores 95+ on all metrics
- **Bundle Efficiency**: JavaScript <100KB, CSS <50KB
- **Build Reliability**: 100% reproducible builds across environments
- **Error Recovery**: <2 minutes average issue resolution with local runners

### User Experience Metrics
- **Developer Onboarding**: Intuitive setup requiring minimal documentation
- **Workflow Efficiency**: Complete local CI/CD pipeline
- **Error Messages**: Clear, actionable error descriptions with recovery steps
- **Documentation Quality**: Comprehensive guides for all workflows

## 📚 Resources & Integration

### Agents.md Workflow Integration
This project follows [agents.md](https://agents.md/) standards:
- **Consistent AI Instructions**: Single source of truth for all AI assistants
- **Symlink Strategy**: CLAUDE.md, GEMINI.md link to AGENTS.md
- **Cross-Platform Compatibility**: Works with all major AI platforms
- **Workflow Standards**: Standardized development and deployment processes

### Related Technologies
- **Astro.build**: https://astro.build/
- **Tailwind CSS**: https://tailwindcss.com/
- **GitHub Pages**: https://pages.github.com/
- **TypeScript**: https://typescriptlang.org/

### Documentation Standards
- **README.md**: Project overview and quick start guide
- **Local Runners**: Comprehensive local CI/CD documentation
- **Performance Guide**: Optimization and monitoring procedures
- **Deployment Guide**: GitHub Pages setup and troubleshooting
- **Contributing Guide**: Development setup and workflow standards

---

## 🔒 Compliance & Verification

### Automated Compliance Checks
```bash
# These checks are MANDATORY before every commit
make ci                          # Complete local CI pipeline
./local-infra/runners/deploy.sh  # Enhanced deployment validation
make status                      # System health verification
```

### Manual Verification Points
1. **GitHub Pages Accessibility**: Live site loads without errors
2. **Performance Metrics**: Lighthouse scores meet targets
3. **TypeScript Compliance**: No type errors in strict mode
4. **Build Reproducibility**: Consistent results across environments
5. **Local CI Consistency**: Local and remote builds match perfectly

---

**CRITICAL**: These requirements are NON-NEGOTIABLE. All AI assistants must follow these guidelines exactly. Failure to comply may result in broken deployments, performance issues, or development workflow disruption.

**Version**: 1.0-2025
**Last Updated**: 2025-09-28
**Status**: ACTIVE - MANDATORY COMPLIANCE
**Target**: Modern web development with zero-cost deployment
**Review**: Required before any major implementation changes