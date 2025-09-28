# Astro Local Runner

> 🚀 **Modern website template with comprehensive local CI/CD workflows and zero-cost GitHub Pages deployment**

A complete development infrastructure for building, testing, and deploying Astro websites without relying on expensive cloud services. Features local-first CI/CD, automated GitHub Pages deployment, and performance optimization.

[![Astro](https://img.shields.io/badge/Astro-5.14.1-FF5D01?style=for-the-badge&logo=astro&logoColor=white)](https://astro.build/)
[![TypeScript](https://img.shields.io/badge/TypeScript-Strict-3178C6?style=for-the-badge&logo=typescript&logoColor=white)](https://www.typescriptlang.org/)
[![Tailwind CSS](https://img.shields.io/badge/Tailwind_CSS-38B2AC?style=for-the-badge&logo=tailwind-css&logoColor=white)](https://tailwindcss.com/)
[![GitHub Pages](https://img.shields.io/badge/GitHub_Pages-222222?style=for-the-badge&logo=github&logoColor=white)](https://pages.github.com/)

## ✨ Features

### 🏗️ Modern Web Development Stack
- **Astro 5.x** - Modern static site generator with island architecture
- **TypeScript Strict Mode** - Full type safety and modern JavaScript features
- **Tailwind CSS** - Utility-first CSS framework for rapid development
- **Performance Optimized** - Lighthouse scores 95+ on all metrics

### 🔧 Local CI/CD Infrastructure
- **Zero-Cost Operations** - Complete local development and testing
- **Local Runners** - Comprehensive build, test, and deployment automation
- **GitHub Pages Ready** - Automatic deployment from `docs/` folder
- **Performance Validation** - Built-in bundle size and optimization checks

### 📋 Development Automation
- **Make-based Workflows** - Simple, reliable automation commands
- **Branch Management** - Timestamped branches with safety workflows
- **Build Verification** - Automatic GitHub Pages compatibility checks
- **Error Recovery** - Built-in troubleshooting and recovery systems

## 🚀 Quick Start

### Prerequisites
- **Node.js 18+** ([Download](https://nodejs.org/))
- **Git** ([Download](https://git-scm.com/))
- **GitHub CLI** (optional, for repository setup): `gh` ([Download](https://cli.github.com/))

### 1. Setup Development Environment
```bash
# Clone and setup
git clone https://github.com/YOUR_USERNAME/astro-local-runner.git
cd astro-local-runner

# Complete environment setup (installs dependencies, configures local CI/CD)
make setup
```

### 2. Start Development
```bash
# Start development server with hot reload
make dev

# In another terminal: Create new feature branch
make branch
# Enter: feat
# Enter: add-new-feature
```

### 3. Development Workflow
```bash
# Edit files in src/
# Changes auto-reload at http://localhost:4321

# Before committing: Run local CI pipeline
make ci
# ✅ TypeScript checks pass
# ✅ Build successful  
# ✅ GitHub Pages compatibility verified
# ✅ Performance validation complete

# Commit and deploy
make commit  # CI validation + commit
make push    # Push to GitHub with auto-deployment
```

## 📖 Documentation

### Local CI/CD Commands

| Command | Description |
|---------|-------------|
| `make setup` | Complete environment setup and dependency installation |
| `make dev` | Start development server with hot reload |
| `make ci` | Run complete local CI pipeline (check + build + verify) |
| `make build` | Build production website to `docs/` folder |
| `make deploy` | Build and prepare for GitHub Pages deployment |
| `make status` | System health check and build verification |
| `make clean` | Clean build artifacts and caches |

### Advanced Workflows

| Command | Description |
|---------|-------------|
| `make branch` | Create timestamped development branch |
| `make commit` | Run CI + commit with validation |
| `make push` | Push to GitHub with deployment preparation |
| `make workflow` | Complete: branch + ci + commit + push |
| `make local-runners` | List all available local CI/CD scripts |

### Local Runner Scripts

```bash
# Enhanced deployment with performance validation
./local-infra/runners/deploy.sh

# GitHub repository setup and Pages configuration  
./local-infra/runners/gh-pages-setup.sh

# Comprehensive build performance analysis
./local-infra/runners/performance-check.sh

# Development server with CI integration
./local-infra/runners/development-server.sh
```

## 🌐 GitHub Pages Deployment

### Automatic Setup
```bash
# Create repository and configure GitHub Pages
./local-infra/runners/gh-pages-setup.sh
# Follow prompts to create repo and enable Pages
```

### Manual Setup
1. **Create GitHub Repository**:
   ```bash
   gh repo create astro-local-runner --public
   git remote add origin https://github.com/YOUR_USERNAME/astro-local-runner.git
   ```

2. **Update Configuration**:
   - Edit `astro.config.mjs`: Update `site` and `base` URLs
   - Edit `package.json`: Update repository URL

3. **Deploy**:
   ```bash
   make deploy  # Build and verify
   git add -A
   git commit -m "Initial deployment"
   git push origin main
   ```

4. **Enable GitHub Pages**:
   - Go to repository Settings → Pages
   - Source: **Deploy from a branch**
   - Branch: **main** 
   - Folder: **/ (docs)**

### Live Site
Your site will be available at: `https://YOUR_USERNAME.github.io/astro-local-runner/`

## 🏗️ Project Structure

```
astro-local-runner/
├── src/
│   ├── layouts/         # Page layouts
│   ├── pages/           # Site pages (routes)
│   ├── components/      # Reusable components
│   └── styles/          # Global styles and themes
├── public/              # Static assets (images, fonts, etc.)
├── docs/                # Built website (GitHub Pages source)
├── local-infra/         # Local CI/CD infrastructure
│   ├── runners/         # Automated workflow scripts
│   ├── logs/            # Operation logs and history
│   └── config/          # Local configuration files
├── .github/
│   └── workflows/       # GitHub Actions (minimal usage)
├── astro.config.mjs     # Astro configuration
├── tailwind.config.mjs  # Tailwind CSS configuration
├── tsconfig.json        # TypeScript configuration (strict mode)
├── package.json         # Dependencies and scripts
├── Makefile            # Local CI/CD automation
├── AGENTS.md           # AI assistant instructions
├── CLAUDE.md           # Symlink to AGENTS.md
├── GEMINI.md           # Symlink to AGENTS.md
└── README.md           # This file
```

## ⚡ Performance & Optimization

### Built-in Optimizations
- **Bundle Splitting** - Automatic code splitting for optimal loading
- **Asset Optimization** - Image compression and format optimization
- **CSS Purging** - Remove unused CSS for smaller bundles
- **Performance Monitoring** - Built-in Lighthouse score validation

### Performance Targets
- **JavaScript Bundles**: <100KB total
- **CSS Files**: <50KB total
- **Build Time**: <30 seconds complete build
- **Hot Reload**: <1 second development changes
- **Lighthouse Scores**: 95+ on all metrics

### Performance Validation
```bash
# Run comprehensive performance analysis
./local-infra/runners/performance-check.sh
# ✅ Bundle size validation
# ✅ Asset optimization check
# ✅ SEO and accessibility validation
# ✅ GitHub Pages compatibility verification
```

## 🔧 Customization

### Adding New Pages
```bash
# Create new page
touch src/pages/about.astro

# Page automatically available at /about/
```

### Adding Components
```bash
# Create reusable component
mkdir -p src/components
touch src/components/Header.astro
```

### Styling with Tailwind
```astro
---
// src/components/Button.astro
export interface Props {
  text: string;
  variant?: 'primary' | 'secondary';
}

const { text, variant = 'primary' } = Astro.props;
---

<button 
  class={`px-4 py-2 rounded font-medium transition-colors ${
    variant === 'primary' 
      ? 'bg-blue-600 hover:bg-blue-700 text-white' 
      : 'bg-gray-200 hover:bg-gray-300 text-gray-800'
  }`}
>
  {text}
</button>
```

### Environment Configuration
```bash
# Development
npm run dev          # Start dev server
make dev            # Enhanced dev server with CI integration

# Production
make ci             # Local CI pipeline
make deploy         # Build and prepare for deployment
```

## 🛡️ Quality Assurance

### Code Quality Standards
- **TypeScript Strict Mode** - Full type safety enforcement
- **ESLint Integration** - Code quality and consistency checking
- **Prettier Support** - Automated code formatting
- **Build Verification** - Automatic deployment compatibility checks

### Testing Strategy
```bash
# Type checking
npm run check       # Astro and TypeScript validation
make ci            # Complete CI pipeline with all checks

# Build testing
make build         # Production build with verification
make verify        # GitHub Pages compatibility check
```

### Error Handling & Recovery
```bash
# System diagnostics
make status        # Comprehensive system health check

# Error recovery
make clean         # Clean all build artifacts
make emergency-reset  # Reset to last known good state
```

## 🤖 AI Assistant Integration

This project follows [agents.md](https://agents.md/) standards for consistent AI assistant instructions:

- **AGENTS.md** - Primary instruction file for all AI assistants
- **CLAUDE.md** - Symlink to AGENTS.md for Claude
- **GEMINI.md** - Symlink to AGENTS.md for Gemini

All AI assistants working on this project will follow the same guidelines, ensuring consistent development practices and quality standards.

## 📚 Resources & Learning

### Framework Documentation
- **Astro**: https://docs.astro.build/
- **Tailwind CSS**: https://tailwindcss.com/docs
- **TypeScript**: https://www.typescriptlang.org/docs/

### Deployment & Hosting
- **GitHub Pages**: https://docs.github.com/pages
- **Custom Domains**: https://docs.github.com/pages/configuring-a-custom-domain-for-your-github-pages-site

### Performance & SEO
- **Lighthouse**: https://developers.google.com/web/tools/lighthouse
- **Web.dev**: https://web.dev/
- **Astro Performance**: https://docs.astro.build/en/guides/performance/

## 🤝 Contributing

### Development Setup
```bash
# Fork the repository and clone
git clone https://github.com/YOUR_USERNAME/astro-local-runner.git
cd astro-local-runner

# Setup development environment
make setup

# Create feature branch
make branch
# Follow the prompts for branch type and description
```

### Code Standards
- Follow TypeScript strict mode requirements
- Use Tailwind CSS for all styling
- Run `make ci` before committing
- Use descriptive commit messages
- Update documentation for new features

### Pull Request Process
1. Run `make ci` to ensure all checks pass
2. Update README if adding new features
3. Add tests for new functionality (when applicable)
4. Ensure GitHub Pages deployment works correctly

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

## 🙋‍♂️ Support

### Common Issues
- **Build Failures**: Run `make clean && make setup && make ci`
- **GitHub Pages 404**: Verify `.nojekyll` file exists in `docs/`
- **Performance Issues**: Run `./local-infra/runners/performance-check.sh`

### Getting Help
- Check the [GitHub Issues](https://github.com/YOUR_USERNAME/astro-local-runner/issues)
- Review the local CI/CD logs in `local-infra/logs/`
- Run `make status` for system diagnostics

---

**Built with ❤️ using Astro, TypeScript, and Tailwind CSS**

*Zero-cost deployment • Local-first development • Performance optimized*
