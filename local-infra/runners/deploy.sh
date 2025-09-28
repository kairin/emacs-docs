#!/bin/bash

# Enhanced deployment script for Astro Local Runner
# Complete local CI/CD with performance validation and GitHub Pages optimization

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
LOG_DIR="$SCRIPT_DIR/../logs"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Ensure log directory exists
mkdir -p "$LOG_DIR"

# Logging function
log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    local color=""

    case "$level" in
        "ERROR") color="$RED" ;;
        "SUCCESS") color="$GREEN" ;;
        "WARNING") color="$YELLOW" ;;
        "INFO") color="$BLUE" ;;
        "STEP") color="$CYAN" ;;
    esac

    echo -e "${color}[$timestamp] [$level] $message${NC}"
    echo "[$timestamp] [$level] $message" >> "$LOG_DIR/deploy-$(date +%s).log"
}

# Performance timing
start_timer() {
    TIMER_START=$(date +%s)
}

end_timer() {
    local operation="$1"
    local end_time=$(date +%s)
    local duration=$((end_time - TIMER_START))
    log "INFO" "$operation completed in ${duration}s"
}

# Change to repository directory
cd "$REPO_DIR"

log "STEP" "Starting Astro Local Runner Deployment Pipeline"
log "INFO" "Repository: $(pwd)"
log "INFO" "Branch: $(git branch --show-current 2>/dev/null || echo 'unknown')"

# Step 1: Environment validation
log "STEP" "Validating environment..."
start_timer

# Check required tools
for tool in node npm git; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        log "ERROR" "$tool is not installed"
        exit 1
    fi
done

# Check Node.js version
node_version=$(node --version | sed 's/v//')
required_version="18.0.0"
if ! npm list semver >/dev/null 2>&1; then
    # Simple version check without semver
    major_version=$(echo "$node_version" | cut -d. -f1)
    if [ "$major_version" -lt 18 ]; then
        log "ERROR" "Node.js version $node_version is too old. Required: >= $required_version"
        exit 1
    fi
fi

log "SUCCESS" "Environment validation passed"
end_timer "Environment validation"

# Step 2: Dependency installation
log "STEP" "Installing/updating dependencies..."
start_timer

npm install

log "SUCCESS" "Dependencies installed"
end_timer "Dependency installation"

# Step 3: TypeScript and Astro checks
log "STEP" "Running TypeScript and Astro checks..."
start_timer

npm run check

log "SUCCESS" "Type checks passed"
end_timer "Type checking"

# Step 4: Build optimization
log "STEP" "Building optimized Astro site..."
start_timer

# Clean previous build
rm -rf ./docs/* ./dist ./.astro 2>/dev/null || true

# Build with detailed output
npm run build

log "SUCCESS" "Build completed"
end_timer "Build process"

# Step 5: Build verification
log "STEP" "Verifying build outputs..."
start_timer

# Check critical files exist
if [ ! -d "./docs" ]; then
    log "ERROR" "docs/ directory not created"
    exit 1
fi

if [ ! -f "./docs/index.html" ]; then
    log "ERROR" "index.html not found in docs/"
    exit 1
fi

if [ ! -f "./docs/.nojekyll" ]; then
    log "ERROR" ".nojekyll file not created (required for GitHub Pages)"
    exit 1
fi

# Count files and calculate size
file_count=$(find ./docs -type f | wc -l)
total_size=$(du -sh ./docs | cut -f1)

log "SUCCESS" "Build verification passed: $file_count files, $total_size total"
end_timer "Build verification"

# Step 6: Performance validation
log "STEP" "Running performance checks..."
start_timer

# Check HTML file sizes (should be reasonable)
large_html_files=$(find ./docs -name "*.html" -size +500k)
if [ -n "$large_html_files" ]; then
    log "WARNING" "Large HTML files detected (>500KB):"
    echo "$large_html_files" | while read -r file; do
        size=$(du -h "$file" | cut -f1)
        log "WARNING" "  $file: $size"
    done
fi

# Check for optimized assets
if [ -d "./docs/_astro" ]; then
    asset_count=$(find ./docs/_astro -type f | wc -l)
    log "INFO" "Optimized assets: $asset_count files in _astro/"
else
    log "WARNING" "_astro directory not found - assets may not be optimized"
fi

log "SUCCESS" "Performance validation completed"
end_timer "Performance validation"

# Step 7: GitHub Pages preparation
log "STEP" "Preparing GitHub Pages deployment..."
start_timer

# Create CNAME file if custom domain is configured
# (This would be customized per project)
# echo "yourdomain.com" > ./docs/CNAME

# Create robots.txt if it doesn't exist
if [ ! -f "./docs/robots.txt" ]; then
    cat > ./docs/robots.txt << 'EOF'
User-agent: *
Allow: /

Sitemap: https://YOUR_USERNAME.github.io/astro-local-runner/sitemap.xml
EOF
    log "INFO" "Created robots.txt"
fi

# Create a deployment manifest
cat > ./docs/deployment-manifest.json << EOF
{
  "deploymentTime": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "buildTool": "Astro Local Runner",
  "cicdType": "Local CI/CD",
  "branch": "$(git branch --show-current 2>/dev/null || echo 'unknown')",
  "commit": "$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')",
  "files": $file_count,
  "totalSize": "$total_size",
  "nodeVersion": "$(node --version)",
  "astroVersion": "$(npm list astro --depth=0 2>/dev/null | grep astro | sed 's/.*astro@//' | sed 's/ .*//' || echo 'unknown')"
}
EOF

log "SUCCESS" "GitHub Pages preparation completed"
end_timer "GitHub Pages preparation"

# Step 8: Final validation and summary
log "STEP" "Final deployment validation..."
start_timer

# Validate all critical files
critical_files=(
    "./docs/index.html"
    "./docs/.nojekyll"
    "./docs/deployment-manifest.json"
)

for file in "${critical_files[@]}"; do
    if [ ! -f "$file" ]; then
        log "ERROR" "Critical file missing: $file"
        exit 1
    fi
done

log "SUCCESS" "All critical files present"
end_timer "Final validation"

# Deployment summary
log "SUCCESS" "🚀 DEPLOYMENT READY!"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Deployment Summary:"
echo "  📁 Files built: $file_count"
echo "  💾 Total size: $total_size"
echo "  🌐 Target: GitHub Pages"
echo "  📂 Source: docs/ folder"
echo ""
echo "📋 Next Steps:"
echo "  1. Review build outputs in ./docs/"
echo "  2. Commit changes: git add -A && git commit -m 'Deploy: $(date)'"
echo "  3. Push to GitHub: git push origin $(git branch --show-current 2>/dev/null || echo 'main')"
echo "  4. Enable GitHub Pages in repo settings (source: docs/ folder)"
echo ""
echo "🌐 Your site will be available at:"
echo "  https://YOUR_USERNAME.github.io/astro-local-runner/"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"