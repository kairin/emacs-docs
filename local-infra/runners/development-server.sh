#!/bin/bash

# Enhanced development server with local CI/CD integration
# Provides hot reload, automatic checks, and development workflow assistance

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
        "DEV") color="$CYAN" ;;
    esac

    echo -e "${color}[$timestamp] [$level] $message${NC}"
}

# Trap to handle cleanup
cleanup() {
    log "INFO" "Shutting down development server..."
    if [ ! -z "${DEV_PID:-}" ]; then
        kill "$DEV_PID" 2>/dev/null || true
    fi
    exit 0
}
trap cleanup INT TERM

# Check if dependencies are installed
log "DEV" "Checking development environment..."
if [ ! -d "node_modules" ]; then
    log "WARNING" "Node modules not found. Installing dependencies..."
    npm install
fi

# Validate package.json scripts
if ! npm run -s >/dev/null 2>&1; then
    log "ERROR" "npm scripts not available. Check package.json"
    exit 1
fi

log "SUCCESS" "Development environment ready!"

# Show available development commands
echo ""
log "DEV" "Astro Local Runner Development Server"
echo "════════════════════════════════════════════════════"
echo ""
echo "🚀 Development Commands:"
echo "  Ctrl+C           Stop development server"
echo "  make check       Run TypeScript checks (in another terminal)"
echo "  make build       Build for production"
echo "  make deploy      Deploy to GitHub Pages"
echo ""
echo "📋 Development Workflow:"
echo "  1. Edit files in src/"
echo "  2. Changes auto-reload in browser"
echo "  3. Run 'make ci' before committing"
echo "  4. Use 'make deploy' to publish"
echo ""
echo "🌐 Local URLs:"

# Start development server in background
log "DEV" "Starting Astro development server..."
npm run dev &
DEV_PID=$!

# Wait a moment for server to start
sleep 3

# Check if server is running
if kill -0 "$DEV_PID" 2>/dev/null; then
    log "SUCCESS" "Development server started successfully!"
    echo ""
    echo "  📍 Local:    http://localhost:4321"
    echo "  📍 Network:  http://$(hostname -I | awk '{print $1}'):4321"
    echo ""
else
    log "ERROR" "Failed to start development server"
    exit 1
fi

# Development assistant loop
echo "════════════════════════════════════════════════════"
log "DEV" "Development server is running. Use Ctrl+C to stop."
echo ""
log "INFO" "Watching for file changes..."

# Monitor for common development needs
while kill -0 "$DEV_PID" 2>/dev/null; do
    sleep 5
    
    # Check if git has uncommitted changes
    if ! git diff --quiet 2>/dev/null; then
        # Only show this occasionally to avoid spam
        if [ $((SECONDS % 60)) -eq 0 ]; then
            log "INFO" "💡 You have uncommitted changes. Run 'make ci' to validate before committing."
        fi
    fi
done

log "WARNING" "Development server stopped unexpectedly"
exit 1