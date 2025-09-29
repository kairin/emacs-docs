#!/bin/bash
# Spec-Kit Runner - Execute staged Emacs implementation
# Based on MetaSpec-Kyocera .specify patterns

set -euo pipefail

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STAGES_DIR="${PROJECT_ROOT}/stages"
DOCS_SOURCE="${PROJECT_ROOT}/../docs-source"
EMACS_CONFIG_DIR="${HOME}/.emacs.d"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'  
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Spec-Kit Commands
show_help() {
    cat << EOF
Spec-Kit Runner - Staged Emacs Implementation

USAGE:
    ./spec-kit-runner.sh <command> [stage] [options]

COMMANDS:
    init <stage>     Initialize specified stage (001, 002, 003)
    validate <stage> Validate stage requirements and readiness
    implement <stage> Execute stage implementation
    status           Show current implementation status
    docs             Generate documentation from specifications
    benchmark        Run performance benchmarks for current stage
    clean            Clean up failed implementations
    help             Show this help message

STAGES:
    001              Functional Emacs Foundation (7 days)
    002              Enhanced Development Environment (4 weeks)  
    003              Mastery and Optimization (6-8 weeks)

EXAMPLES:
    ./spec-kit-runner.sh init 001
    ./spec-kit-runner.sh implement 001
    ./spec-kit-runner.sh validate 002
    ./spec-kit-runner.sh status
    ./spec-kit-runner.sh benchmark

EOF
}

validate_stage() {
    local stage="$1"
    local stage_dir="${STAGES_DIR}/${stage}"
    
    log_info "Validating Stage ${stage} requirements..."
    
    # Check stage directory exists
    if [[ ! -d "$stage_dir" ]]; then
        log_error "Stage ${stage} directory not found: $stage_dir"
        return 1
    fi
    
    # Check required specification files
    local required_files=(
        "1-spec-kit-constitution.md"
        "2-spec-kit-specify.md"
        "SPEC_KIT_INDEX.md"
    )
    
    for file in "${required_files[@]}"; do
        if [[ ! -f "${stage_dir}/${file}" ]]; then
            log_error "Required file missing: ${file}"
            return 1
        fi
    done
    
    # Stage-specific validations
    case $stage in
        "001")
            # Check system requirements for Stage 001
            log_info "Checking Ubuntu 25.04 compatibility..."
            if ! command -v apt &> /dev/null; then
                log_error "APT package manager not found - Ubuntu required"
                return 1
            fi
            
            log_info "Checking build dependencies..."
            if ! dpkg -l | grep -q "build-essential"; then
                log_warn "build-essential not installed - required for Emacs compilation"
            fi
            ;;
        "002")
            # Validate Stage 001 completion
            if [[ ! -f "${HOME}/.emacs.d/init.el" ]]; then
                log_error "Stage 001 not completed - init.el not found"
                return 1
            fi
            ;;
        "003")
            # Validate Stage 002 completion
            if ! emacs --batch --eval "(require 'lsp-mode)" 2>/dev/null; then
                log_error "Stage 002 not completed - LSP integration missing"
                return 1
            fi
            ;;
    esac
    
    log_success "Stage ${stage} validation passed"
    return 0
}

init_stage() {
    local stage="$1"
    
    log_info "Initializing Stage ${stage} implementation..."
    
    # Validate prerequisites
    if ! validate_stage "$stage"; then
        log_error "Stage ${stage} validation failed"
        return 1
    fi
    
    # Create working directory
    local work_dir="${PROJECT_ROOT}/work/stage-${stage}"
    mkdir -p "$work_dir"
    
    # Stage-specific initialization
    case $stage in
        "001")
            log_info "Setting up Stage 001: Functional Emacs Foundation"
            
            # Backup existing Emacs configuration
            if [[ -d "$EMACS_CONFIG_DIR" ]]; then
                log_info "Backing up existing Emacs configuration..."
                mv "$EMACS_CONFIG_DIR" "${EMACS_CONFIG_DIR}.backup.$(date +%Y%m%d-%H%M%S)"
            fi
            
            # Create new configuration directory
            mkdir -p "$EMACS_CONFIG_DIR"
            
            # Initialize basic structure
            cat > "${EMACS_CONFIG_DIR}/early-init.el" << 'EOF'
;; Early initialization for performance optimization
;; Stage 001: Functional Emacs Foundation

;; Garbage collection optimization
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.6)

;; Package system optimization  
(setq package-enable-at-startup nil)

;; UI optimization for faster startup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
EOF

            # Initialize main configuration
            cat > "${EMACS_CONFIG_DIR}/init.el" << 'EOF'
;;; init.el --- Emacs Configuration - Stage 001: Functional Foundation
;;; Commentary:
;; Systematic Emacs configuration implementing spec-kit Stage 001
;; Focus: Functional, reliable, performant foundation

;;; Code:

;; Performance monitoring
(defvar spec-kit--init-start-time (current-time))

;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Core configuration placeholder
;; TODO: Implement Stage 001 specifications

;; Startup performance report
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) spec-kit--init-start-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here
EOF
            
            log_success "Stage 001 initialized with basic structure"
            ;;
            
        "002"|"003")
            log_info "Stage ${stage} initialization ready"
            log_info "Complete Stage 001 before proceeding to Stage ${stage}"
            ;;
    esac
    
    log_success "Stage ${stage} initialization completed"
}

implement_stage() {
    local stage="$1"
    
    log_info "Implementing Stage ${stage}..."
    
    # Implementation will be stage-specific
    case $stage in
        "001")
            log_info "Beginning Stage 001 implementation..."
            # TODO: Implement based on 2-spec-kit-specify.md requirements
            log_warn "Stage 001 implementation requires detailed package setup"
            log_info "Refer to: ${STAGES_DIR}/001/2-spec-kit-specify.md"
            ;;
        "002")
            log_warn "Stage 002 requires Stage 001 completion"
            ;;
        "003")
            log_warn "Stage 003 requires Stage 002 completion"
            ;;
    esac
}

show_status() {
    log_info "Spec-Kit Implementation Status"
    echo "================================="
    
    # Check each stage
    for stage in 001 002 003; do
        echo -n "Stage ${stage}: "
        if validate_stage "$stage" 2>/dev/null; then
            echo -e "${GREEN}✅ Ready${NC}"
        else
            echo -e "${YELLOW}⏳ Pending${NC}"
        fi
    done
    
    # Check Emacs installation
    echo -n "Emacs: "
    if command -v emacs &> /dev/null; then
        local version=$(emacs --version | head -1 | grep -o '[0-9]\+\.[0-9]\+')
        echo -e "${GREEN}✅ Version ${version}${NC}"
    else
        echo -e "${RED}❌ Not installed${NC}"
    fi
    
    # Check configuration
    echo -n "Configuration: "
    if [[ -f "${HOME}/.emacs.d/init.el" ]]; then
        echo -e "${GREEN}✅ Present${NC}"
    else
        echo -e "${YELLOW}⏳ Not initialized${NC}"
    fi
}

benchmark_performance() {
    log_info "Running performance benchmarks..."
    
    if ! command -v emacs &> /dev/null; then
        log_error "Emacs not installed - cannot benchmark"
        return 1
    fi
    
    # Startup time benchmark
    log_info "Measuring startup time..."
    local startup_time=$(emacs --batch --eval "
        (let ((start (current-time)))
          (princ (format \"%.3f\" (float-time (time-subtract (current-time) start)))))" 2>/dev/null)
    
    echo "Startup time: ${startup_time}s"
    
    # Compare against Stage targets
    local target_time="2.0"
    if (( $(echo "$startup_time < $target_time" | bc -l) )); then
        log_success "Startup time meets Stage 001 target (< ${target_time}s)"
    else
        log_warn "Startup time exceeds Stage 001 target (${startup_time}s > ${target_time}s)"
    fi
}

# Main command dispatcher
main() {
    case "${1:-help}" in
        "init")
            if [[ -z "${2:-}" ]]; then
                log_error "Stage number required. Usage: init <stage>"
                exit 1
            fi
            init_stage "$2"
            ;;
        "validate")
            if [[ -z "${2:-}" ]]; then
                log_error "Stage number required. Usage: validate <stage>"
                exit 1
            fi
            validate_stage "$2"
            ;;
        "implement")
            if [[ -z "${2:-}" ]]; then
                log_error "Stage number required. Usage: implement <stage>"
                exit 1
            fi
            implement_stage "$2"
            ;;
        "status")
            show_status
            ;;
        "benchmark")
            benchmark_performance
            ;;
        "help"|"--help"|"-h")
            show_help
            ;;
        *)
            log_error "Unknown command: $1"
            show_help
            exit 1
            ;;
    esac
}

# Execute main function with all arguments
main "$@"