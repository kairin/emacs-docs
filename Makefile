# Astro Local Runner - Comprehensive Local CI/CD Makefile
# Zero-cost local development and deployment workflows
# Inspired by MCP Manager and MetaSpec-Kyocera best practices

.PHONY: help setup build deploy status clean test dev preview local-runners

# Default target
help: ## Show this help message
	@echo "Astro Local Runner - Local CI/CD Automation"
	@echo "=========================================="
	@echo ""
	@echo "🚀 Quick Start:"
	@echo "  make setup    # Initial setup (install dependencies)"
	@echo "  make dev      # Start development server"
	@echo "  make ci       # Run complete local CI pipeline"
	@echo "  make deploy   # Build and deploy to GitHub Pages"
	@echo ""
	@echo "📋 Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "🔧 Local Runners Available:"
	@ls -la local-infra/runners/ 2>/dev/null | grep -E "\.(sh)$$" | wc -l | xargs -I {} echo "  {} runner scripts available"
	@echo ""
	@echo "📍 Repository Setup:"
	@echo "  1. Create GitHub repo: gh repo create astro-local-runner --public"
	@echo "  2. Enable GitHub Pages in repo settings (source: docs/ folder)"
	@echo "  3. Run: make setup && make deploy"

setup: ## Initial project setup (install all dependencies)
	@echo "📦 Setting up Astro Local Runner..."
	@echo "🔧 Installing Node.js dependencies..."
	npm install
	@echo "🔧 Setting up local CI/CD infrastructure..."
	chmod +x local-infra/runners/*.sh
	@echo "📋 Verifying system requirements..."
	@which node >/dev/null || (echo "❌ Node.js not found. Install Node.js 18+" && exit 1)
	@which npm >/dev/null || (echo "❌ npm not found. Install Node.js 18+" && exit 1)
	@which git >/dev/null || (echo "❌ Git not found. Install Git" && exit 1)
	@echo "✅ Setup complete! Run 'make dev' to start development"

branch: ## Create new timestamped branch with proper naming
	@echo "🌿 Creating new development branch..."
	@DATETIME=$$(date +"%Y%m%d-%H%M%S"); \
	read -p "Enter type (feat/fix/docs/refactor/test/chore): " TYPE; \
	read -p "Enter short description (lowercase, hyphens): " DESC; \
	BRANCH="$$DATETIME-$$TYPE-$$DESC"; \
	git checkout -b "$$BRANCH"; \
	echo "✅ Created branch: $$BRANCH"

check: ## Run Astro and TypeScript checks
	@echo "🔍 Running TypeScript and Astro checks..."
	npm run astro check
	@echo "✅ Type checks passed!"

format: ## Auto-format code (if formatter is available)
	@echo "🎨 Formatting code..."
	@if command -v prettier >/dev/null 2>&1; then \
		npx prettier --write "src/**/*.{js,jsx,ts,tsx,astro,json,css,md}"; \
	else \
		echo "⚠️ Prettier not installed. Run: npm install -D prettier"; \
	fi
	@echo "✅ Code formatted!"

test: ## Run tests (if configured)
	@echo "🧪 Running tests..."
	@if [ -f "vitest.config.js" ] || [ -f "vitest.config.ts" ]; then \
		npm run test; \
	else \
		echo "⚠️ No tests configured. This is normal for a basic Astro setup."; \
	fi
	@echo "✅ Tests completed!"

build: ## Build Astro website to docs/ folder
	@echo "🏗️ Building Astro website..."
	npm run build
	@echo "✅ Website built to ./docs/"

verify: ## Verify build outputs are ready for GitHub Pages
	@echo "🔍 Verifying build outputs..."
	@test -d ./docs || (echo "❌ docs/ directory not found" && exit 1)
	@test -f ./docs/.nojekyll || (echo "❌ .nojekyll file not found" && exit 1)
	@test -f ./docs/index.html || (echo "❌ index.html not found" && exit 1)
	@FILE_COUNT=$$(find ./docs -type f | wc -l); \
	echo "✅ Verified: $$FILE_COUNT files ready for deployment"

status: ## Check local CI/CD system status
	@echo "📊 Astro Local Runner System Status:"
	@echo "===================================="
	@echo "📂 Project Directory: $$(pwd)"
	@echo "🌿 Current Branch: $$(git branch --show-current 2>/dev/null || echo 'Unknown')"
	@echo "📦 Node.js: $$(node --version 2>/dev/null || echo 'Not found')"
	@echo "📦 npm: $$(npm --version 2>/dev/null || echo 'Not found')"
	@echo "🔧 Git: $$(git --version 2>/dev/null || echo 'Not found')"
	@echo ""
	@echo "📋 Build Status:"
	@if [ -d "./docs" ]; then \
		FILE_COUNT=$$(find ./docs -type f | wc -l); \
		echo "  ✅ Built: $$FILE_COUNT files in docs/"; \
	else \
		echo "  ❌ Not built - run 'make build'"; \
	fi
	@echo ""
	@echo "🌐 Remote Status:"
	@REMOTE_URL=$$(git remote get-url origin 2>/dev/null || echo "Not configured"); \
	echo "  📍 Remote URL: $$REMOTE_URL"
	@if echo "$$REMOTE_URL" | grep -q "github.com"; then \
		REPO_NAME=$$(basename "$$REMOTE_URL" .git); \
		OWNER=$$(echo "$$REMOTE_URL" | sed 's/.*github.com[:/]\([^/]*\)\/.*/\1/'); \
		echo "  🚀 GitHub Pages: https://$$OWNER.github.io/$$REPO_NAME/"; \
	fi

ci: ## Run complete local CI pipeline
	@echo ""
	@echo "🚀 Starting Local CI Pipeline..."
	@echo "================================"
	@$(MAKE) check
	@$(MAKE) test
	@$(MAKE) build
	@$(MAKE) verify
	@echo ""
	@echo "✨ Local CI Pipeline Complete!"
	@echo "=============================="
	@echo "🎉 All checks passed! Ready to deploy."

deploy: ## Build and prepare for deployment (requires manual git push)
	@echo "🚀 Preparing deployment..."
	@$(MAKE) ci
	@echo ""
	@echo "📋 Deployment Ready! Next steps:"
	@echo "1. git add -A"
	@echo "2. git commit -m 'Deploy: $$(date)'"
	@echo "3. git push origin $$(git branch --show-current)"
	@echo ""
	@echo "🌐 GitHub Pages will automatically deploy from docs/ folder"

commit: ## Run CI and commit changes with timestamp
	@echo "💾 Preparing commit..."
	@$(MAKE) ci
	@echo ""
	@read -p "Enter commit message: " MSG; \
	git add -A; \
	git commit -m "$$MSG - $$(date)"; \
	echo "✅ Changes committed! Run 'make push' to push to remote."

push: ## Push current branch to remote
	@BRANCH=$$(git branch --show-current); \
	echo "🚀 Pushing $$BRANCH to remote..."; \
	git push origin "$$BRANCH"; \
	echo "✅ Pushed! GitHub Pages will deploy automatically."

workflow: ## Complete workflow: branch, ci, commit, push
	@$(MAKE) branch
	@$(MAKE) ci
	@$(MAKE) commit
	@$(MAKE) push
	@echo ""
	@echo "✅ Complete workflow executed!"
	@echo "📌 GitHub Pages deployment in progress..."

clean: ## Clean build artifacts and caches
	@echo "🧹 Cleaning build artifacts..."
	rm -rf ./docs/* || true
	rm -rf ./dist || true
	rm -rf ./.astro || true
	rm -rf ./node_modules/.cache || true
	@echo "✅ Clean complete!"

dev: ## Start Astro development server
	@echo "🚀 Starting Astro development server..."
	npm run dev

preview: ## Preview built site locally
	@echo "👀 Previewing built site..."
	npm run preview

local-runners: ## List and verify all local CI/CD runners
	@echo "🛠️ Astro Local Runner CI/CD Infrastructure:"
	@echo "==========================================="
	@ls -la local-infra/runners/ 2>/dev/null | grep -E "\.(sh)$$" | while read -r line; do \
		file=$$(echo "$$line" | awk '{print $$NF}'); \
		echo "  📜 $$file - $$(head -2 "local-infra/runners/$$file" | tail -1 | sed 's/^# //')"; \
	done
	@echo ""
	@echo "🔧 Available runner commands:"
	@echo "  ./local-infra/runners/deploy.sh           - Enhanced deployment"
	@echo "  ./local-infra/runners/gh-pages-setup.sh   - GitHub Pages configuration"
	@echo "  ./local-infra/runners/performance-check.sh - Performance validation"

# Emergency recovery
emergency-reset: ## Emergency reset to last known good state
	@echo "🚨 Emergency reset - restoring to main branch..."
	git stash
	git checkout main || git checkout master
	git reset --hard origin/main || git reset --hard origin/master
	make clean
	@echo "✅ Emergency reset complete"

# GitHub repository creation helper
create-repo: ## Create GitHub repository and set up Pages
	@echo "📍 Creating GitHub repository..."
	@read -p "Enter repository name [astro-local-runner]: " REPO_NAME; \
	REPO_NAME=$${REPO_NAME:-astro-local-runner}; \
	read -p "Enter description: " DESCRIPTION; \
	gh repo create "$$REPO_NAME" --public --description "$$DESCRIPTION" --clone=false; \
	git remote add origin "https://github.com/$$(gh auth status 2>&1 | grep 'Logged in' | sed 's/.*as \([^)]*\).*/\1')/$$REPO_NAME.git"; \
	echo "✅ Repository created! Now run 'make setup && make deploy'"

# Default goal
.DEFAULT_GOAL := help