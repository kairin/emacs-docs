#!/bin/bash

# Performance validation script for Astro Local Runner
# Checks build output for optimization and performance best practices

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
        "CHECK") color="$CYAN" ;;
    esac

    echo -e "${color}[$timestamp] [$level] $message${NC}"
}

# Check if docs directory exists
if [ ! -d "./docs" ]; then
    log "ERROR" "docs/ directory not found. Run 'make build' first."
    exit 1
fi

log "CHECK" "Starting performance validation for Astro Local Runner"
echo ""

# Performance metrics tracking
TOTAL_FILES=0
TOTAL_SIZE=0
WARNINGS=0
ERRORS=0

# Check 1: Critical files existence
log "CHECK" "Validating critical files..."
critical_files=(
    "./docs/index.html"
    "./docs/.nojekyll"
)

for file in "${critical_files[@]}"; do
    if [ -f "$file" ]; then
        size=$(du -h "$file" | cut -f1)
        log "SUCCESS" "✓ $file ($size)"
    else
        log "ERROR" "✗ Missing: $file"
        ((ERRORS++))
    fi
done

# Check 2: HTML file optimization
log "CHECK" "Analyzing HTML files..."
html_files=$(find ./docs -name "*.html" -type f)
html_count=0
large_html_files=0

for file in $html_files; do
    ((html_count++))
    size_bytes=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
    size_kb=$((size_bytes / 1024))
    
    if [ $size_kb -gt 500 ]; then
        size_h=$(du -h "$file" | cut -f1)
        log "WARNING" "Large HTML file: $file ($size_h)"
        ((large_html_files++))
        ((WARNINGS++))
    fi
done

log "INFO" "HTML files: $html_count total, $large_html_files large (>500KB)"

# Check 3: Asset optimization
log "CHECK" "Analyzing assets..."
if [ -d "./docs/_astro" ]; then
    asset_files=$(find ./docs/_astro -type f)
    asset_count=0
    css_files=0
    js_files=0
    image_files=0
    
    for file in $asset_files; do
        ((asset_count++))
        case "$file" in
            *.css) ((css_files++)) ;;
            *.js) ((js_files++)) ;;
            *.png|*.jpg|*.jpeg|*.gif|*.webp|*.svg) ((image_files++)) ;;
        esac
    done
    
    log "SUCCESS" "Optimized assets: $asset_count files"
    log "INFO" "  CSS files: $css_files"
    log "INFO" "  JS files: $js_files"
    log "INFO" "  Images: $image_files"
else
    log "WARNING" "_astro directory not found - assets may not be optimized"
    ((WARNINGS++))
fi

# Check 4: JavaScript bundle analysis
log "CHECK" "Analyzing JavaScript bundles..."
js_files=$(find ./docs -name "*.js" -type f)
large_js_files=0
total_js_size=0

for file in $js_files; do
    size_bytes=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
    size_kb=$((size_bytes / 1024))
    total_js_size=$((total_js_size + size_kb))
    
    if [ $size_kb -gt 100 ]; then
        size_h=$(du -h "$file" | cut -f1)
        log "WARNING" "Large JS bundle: $file ($size_h)"
        ((large_js_files++))
        ((WARNINGS++))
    fi
done

log "INFO" "JavaScript: ${total_js_size}KB total, $large_js_files large bundles (>100KB)"

# Check 5: CSS optimization
log "CHECK" "Analyzing CSS files..."
css_files=$(find ./docs -name "*.css" -type f)
css_count=0
total_css_size=0

for file in $css_files; do
    ((css_count++))
    size_bytes=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
    size_kb=$((size_bytes / 1024))
    total_css_size=$((total_css_size + size_kb))
    
    if [ $size_kb -gt 50 ]; then
        size_h=$(du -h "$file" | cut -f1)
        log "INFO" "CSS file: $file ($size_h)"
    fi
done

log "INFO" "CSS: ${total_css_size}KB total in $css_count files"

# Check 6: Image optimization
log "CHECK" "Analyzing images..."
image_extensions="png jpg jpeg gif webp svg ico"
image_count=0
large_images=0
total_image_size=0

for ext in $image_extensions; do
    images=$(find ./docs -name "*.$ext" -type f 2>/dev/null || true)
    for file in $images; do
        if [ -n "$file" ]; then
            ((image_count++))
            size_bytes=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
            size_kb=$((size_bytes / 1024))
            total_image_size=$((total_image_size + size_kb))
            
            if [ $size_kb -gt 1000 ]; then
                size_h=$(du -h "$file" | cut -f1)
                log "WARNING" "Large image: $file ($size_h)"
                ((large_images++))
                ((WARNINGS++))
            fi
        fi
    done
done

log "INFO" "Images: ${total_image_size}KB total in $image_count files, $large_images large (>1MB)"

# Check 7: Compression opportunities
log "CHECK" "Checking compression opportunities..."
compressible_extensions="html css js svg"
uncompressed_size=0
can_compress=0

for ext in $compressible_extensions; do
    files=$(find ./docs -name "*.$ext" -type f 2>/dev/null || true)
    for file in $files; do
        if [ -n "$file" ]; then
            size_bytes=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
            size_kb=$((size_bytes / 1024))
            uncompressed_size=$((uncompressed_size + size_kb))
            
            if [ $size_kb -gt 20 ]; then
                ((can_compress++))
            fi
        fi
    done
done

log "INFO" "Compressible files: ${uncompressed_size}KB in $can_compress files (>20KB)"

# Check 8: SEO and accessibility basics
log "CHECK" "Validating SEO and accessibility..."
if [ -f "./docs/index.html" ]; then
    # Check for basic SEO elements
    if grep -q "<title>" "./docs/index.html"; then
        log "SUCCESS" "✓ Title tag found"
    else
        log "WARNING" "Title tag missing"
        ((WARNINGS++))
    fi
    
    if grep -q '<meta name="description"' "./docs/index.html"; then
        log "SUCCESS" "✓ Meta description found"
    else
        log "WARNING" "Meta description missing"
        ((WARNINGS++))
    fi
    
    if grep -q '<meta name="viewport"' "./docs/index.html"; then
        log "SUCCESS" "✓ Viewport meta tag found"
    else
        log "WARNING" "Viewport meta tag missing"
        ((WARNINGS++))
    fi
fi

# Check 9: GitHub Pages compatibility
log "CHECK" "Validating GitHub Pages compatibility..."
if [ -f "./docs/.nojekyll" ]; then
    log "SUCCESS" "✓ .nojekyll file present"
else
    log "ERROR" ".nojekyll file missing (required for GitHub Pages)"
    ((ERRORS++))
fi

# Check for CNAME if custom domain
if [ -f "./docs/CNAME" ]; then
    domain=$(cat "./docs/CNAME")
    log "INFO" "Custom domain configured: $domain"
fi

# Calculate total build size
total_size_kb=0
all_files=$(find ./docs -type f)
for file in $all_files; do
    size_bytes=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
    size_kb=$((size_bytes / 1024))
    total_size_kb=$((total_size_kb + size_kb))
done

total_size_mb=$((total_size_kb / 1024))

# Performance score calculation
SCORE=100
SCORE=$((SCORE - ERRORS * 20))  # Major penalty for errors
SCORE=$((SCORE - WARNINGS * 5)) # Minor penalty for warnings

if [ $total_size_mb -gt 50 ]; then
    SCORE=$((SCORE - 10)) # Penalty for large sites
fi

if [ $SCORE -lt 0 ]; then
    SCORE=0
fi

# Final report
echo ""
log "CHECK" "Performance Validation Report"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Set color based on score
if [ $SCORE -ge 90 ]; then
    score_color="$GREEN"
elif [ $SCORE -ge 70 ]; then
    score_color="$YELLOW"
else
    score_color="$RED"
fi

echo -e "${score_color}Performance Score: $SCORE/100${NC}"
echo ""
echo "📊 Build Statistics:"
echo "  📁 Total size: ${total_size_mb}MB (${total_size_kb}KB)"
echo "  📄 HTML files: $html_count"
echo "  🎨 CSS size: ${total_css_size}KB"
echo "  ⚡ JavaScript size: ${total_js_size}KB"
echo "  🖼️  Images: ${total_image_size}KB"
echo ""
echo "⚠️  Issues Found:"
echo "  🚨 Errors: $ERRORS"
echo "  ⚠️  Warnings: $WARNINGS"
echo ""
echo "📋 Recommendations:"

if [ $total_size_mb -gt 50 ]; then
    echo "  • Consider reducing total site size (currently ${total_size_mb}MB)"
fi

if [ $large_js_files -gt 0 ]; then
    echo "  • Optimize JavaScript bundles (${large_js_files} large files)"
fi

if [ $large_images -gt 0 ]; then
    echo "  • Optimize images (${large_images} large files >1MB)"
fi

if [ $can_compress -gt 10 ]; then
    echo "  • Enable gzip compression for better performance"
fi

echo "  • Consider adding a sitemap.xml"
echo "  • Add favicon.ico if not present"
echo "  • Validate HTML with W3C validator"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Exit with appropriate code
if [ $ERRORS -gt 0 ]; then
    log "ERROR" "Performance validation failed with $ERRORS errors"
    exit 1
elif [ $WARNINGS -gt 5 ]; then
    log "WARNING" "Performance validation completed with $WARNINGS warnings"
    exit 0
else
    log "SUCCESS" "Performance validation passed!"
    exit 0
fi