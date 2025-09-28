#!/bin/bash

# GitHub Pages setup and configuration script
# Automates repository creation and Pages configuration

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%H:%M:%S')
    local color=""

    case "$level" in
        "ERROR") color="$RED" ;;
        "SUCCESS") color="$GREEN" ;;
        "WARNING") color="$YELLOW" ;;
        "INFO") color="$BLUE" ;;
        "STEP") color="$CYAN" ;;
    esac

    echo -e "${color}[$timestamp] [$level] $message${NC}"
}

# Check if gh CLI is available
if ! command -v gh >/dev/null 2>&1; then
    log "ERROR" "GitHub CLI (gh) is not installed"
    log "INFO" "Install it from: https://cli.github.com/"
    log "INFO" "Or run: sudo apt install gh (on Ubuntu/Debian)"
    exit 1
fi

# Check if user is authenticated
if ! gh auth status >/dev/null 2>&1; then
    log "ERROR" "Not authenticated with GitHub CLI"
    log "INFO" "Run: gh auth login"
    exit 1
fi

log "STEP" "GitHub Pages Setup for Astro Local Runner"

# Get repository information
REPO_NAME="astro-local-runner"
CURRENT_USER=$(gh auth status 2>&1 | grep 'Logged in' | sed 's/.*as \([^)]*\).*/\1/')

echo ""
log "INFO" "Current GitHub user: $CURRENT_USER"
log "INFO" "Default repo name: $REPO_NAME"
echo ""

# Ask for repository details
read -p "Repository name [$REPO_NAME]: " input_repo_name
REPO_NAME=${input_repo_name:-$REPO_NAME}

read -p "Repository description [Astro website with local CI/CD workflows]: " REPO_DESC
REPO_DESC=${REPO_DESC:-"Astro website with local CI/CD workflows"}

read -p "Make repository public? [Y/n]: " make_public
make_public=${make_public:-Y}

if [[ $make_public =~ ^[Yy]$ ]]; then
    VISIBILITY="--public"
else
    VISIBILITY="--private"
fi

# Create repository
log "STEP" "Creating GitHub repository..."
if gh repo create "$REPO_NAME" $VISIBILITY --description "$REPO_DESC" --clone=false; then
    log "SUCCESS" "Repository created: https://github.com/$CURRENT_USER/$REPO_NAME"
else
    log "WARNING" "Repository might already exist. Continuing..."
fi

# Set up remote
log "STEP" "Configuring git remote..."
REMOTE_URL="https://github.com/$CURRENT_USER/$REPO_NAME.git"

# Remove existing remote if it exists
git remote remove origin 2>/dev/null || true

# Add new remote
git remote add origin "$REMOTE_URL"
log "SUCCESS" "Remote configured: $REMOTE_URL"

# Update astro.config.mjs with correct URLs
log "STEP" "Updating Astro configuration..."
CONFIG_FILE="astro.config.mjs"
if [ -f "$CONFIG_FILE" ]; then
    # Update site and base URLs
    sed -i "s|site: 'https://YOUR_USERNAME.github.io'|site: 'https://$CURRENT_USER.github.io'|g" "$CONFIG_FILE"
    sed -i "s|base: '/astro-local-runner'|base: '/$REPO_NAME'|g" "$CONFIG_FILE"
    log "SUCCESS" "Updated astro.config.mjs with correct URLs"
else
    log "WARNING" "astro.config.mjs not found"
fi

# Update package.json repository URL
log "STEP" "Updating package.json..."
PACKAGE_JSON="package.json"
if [ -f "$PACKAGE_JSON" ]; then
    sed -i "s|https://github.com/YOUR_USERNAME/astro-local-runner.git|$REMOTE_URL|g" "$PACKAGE_JSON"
    log "SUCCESS" "Updated package.json repository URL"
fi

# Create initial commit if needed
log "STEP" "Preparing initial commit..."
git add -A
if git diff --staged --quiet; then
    log "INFO" "No changes to commit"
else
    git commit -m "Initial commit: Astro Local Runner setup

- Complete local CI/CD infrastructure
- GitHub Pages deployment ready
- Enhanced build and deployment scripts
- Performance optimization configured"
    log "SUCCESS" "Initial commit created"
fi

# Push to GitHub
log "STEP" "Pushing to GitHub..."
CURRENT_BRANCH=$(git branch --show-current)
git push -u origin "$CURRENT_BRANCH"
log "SUCCESS" "Pushed to GitHub"

# Enable GitHub Pages (requires GitHub API or manual setup)
log "STEP" "Setting up GitHub Pages..."
log "INFO" "Attempting to configure GitHub Pages via CLI..."

# Try to enable Pages via gh CLI (requires newer version)
if gh api "repos/$CURRENT_USER/$REPO_NAME/pages" -X POST -f source='{"branch":"'$CURRENT_BRANCH'","path":"/docs"}' 2>/dev/null; then
    log "SUCCESS" "GitHub Pages enabled automatically!"
else
    log "WARNING" "Could not enable Pages automatically. Manual setup required:"
    log "INFO" "1. Go to: https://github.com/$CURRENT_USER/$REPO_NAME/settings/pages"
    log "INFO" "2. Set Source to: Deploy from a branch"
    log "INFO" "3. Set Branch to: $CURRENT_BRANCH"
    log "INFO" "4. Set Folder to: /docs"
    log "INFO" "5. Click Save"
fi

# Create GitHub Actions workflow for enhanced deployment (optional)
log "STEP" "Creating GitHub Actions workflow..."
mkdir -p .github/workflows

cat > .github/workflows/pages.yml << EOF
# Deploy Astro site to GitHub Pages
# This workflow deploys the docs/ folder built by local CI/CD
name: Deploy to GitHub Pages

on:
  push:
    branches: [main, master]
    paths: ['docs/**']
  workflow_dispatch:

# Sets permissions of the GITHUB_TOKEN
permissions:
  contents: read
  pages: write
  id-token: write

# Allow only one concurrent deployment
concurrency:
  group: "pages"
  cancel-in-progress: false

jobs:
  deploy:
    environment:
      name: github-pages
      url: \${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Pages
        uses: actions/configure-pages@v5

      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: ./docs

      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
EOF

git add .github/workflows/pages.yml
git commit -m "Add GitHub Actions workflow for Pages deployment" || true
git push || true

log "SUCCESS" "GitHub Actions workflow created"

# Final summary
echo ""
log "SUCCESS" "🎉 GitHub Pages Setup Complete!"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Setup Summary:"
echo "  📍 Repository: https://github.com/$CURRENT_USER/$REPO_NAME"
echo "  🌐 GitHub Pages: https://$CURRENT_USER.github.io/$REPO_NAME/"
echo "  🔧 Settings: https://github.com/$CURRENT_USER/$REPO_NAME/settings/pages"
echo ""
echo "🚀 Next Steps:"
echo "  1. Build your site: make build"
echo "  2. Deploy: make deploy"
echo "  3. Visit your live site in 2-3 minutes!"
echo ""
echo "💡 Development Workflow:"
echo "  make dev     # Start development server"
echo "  make ci      # Run local CI pipeline"
echo "  make deploy  # Build and prepare for deployment"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"