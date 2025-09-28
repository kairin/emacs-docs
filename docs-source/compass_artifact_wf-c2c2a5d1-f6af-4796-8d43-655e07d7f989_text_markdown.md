# Complete Emacs Setup Guide for Ubuntu 25.04: Transitioning from Ghostty with AI Integration

## Getting started with Emacs 30 on Ubuntu 25.04

This comprehensive guide will transform your Ubuntu 25.04 system into an AI-powered development environment using Emacs, helping you transition from Ghostty terminal while integrating Claude Code, GitHub Copilot CLI, and Google Gemini CLI.

## 1. Initial Emacs Installation and Setup

### Best Version for Ubuntu 25.04

**Emacs 30.2** is recommended, offering **2.5-5x performance improvement** through native compilation, built-in JSON parsing, and enhanced Tree-sitter support. The native compilation feature compiles Elisp to machine code automatically, providing significant performance benefits especially for AI workloads.

### Essential Ubuntu Packages

```bash
# Enable source repositories
sudo sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list
sudo apt update

# Install build dependencies
sudo apt install build-essential
sudo apt build-dep emacs

# Install additional packages for modern features
sudo apt install \
    libgccjit0 libgccjit-12-dev gcc-12 \
    libjansson4 libjansson-dev \
    libtree-sitter-dev \
    libwebp-dev imagemagick \
    libxft-dev gnutls-bin \
    texinfo git cmake libtool-bin
```

### Installation Methods

**Method 1: PPA (Easiest for beginners)**
```bash
sudo add-apt-repository ppa:ubuntuhandbook1/emacs
sudo apt update && sudo apt install emacs-gtk
```

**Method 2: Build from Source (Best performance)**
```bash
export CC=/usr/bin/gcc-12
mkdir ~/src && cd ~/src
git clone git://git.savannah.gnu.org/emacs.git
cd emacs && git checkout emacs-30

./autogen.sh
./configure --with-native-compilation=aot \
            --with-imagemagick --with-json \
            --with-tree-sitter --with-xft
make -j$(nproc) && sudo make install
```

### Initial Configuration Structure

**~/.emacs.d/early-init.el** (Performance optimizations):
```elisp
;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Maximize garbage collection threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable package.el in favor of modern package managers
(setq package-enable-at-startup nil)

;; Remove GUI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-jobs-number (max 1 (- (num-processors) 2))
        native-comp-speed 2))

;;; early-init.el ends here
```

**~/.emacs.d/init.el** (Main configuration):
```elisp
;;; init.el --- Main configuration -*- lexical-binding: t -*-

;; Reset garbage collection after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.2)))

;; Package management setup
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("NonGNU ELPA"  . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;; Basic improvements
(setq inhibit-splash-screen t
      ring-bell-function 'ignore)
(global-auto-revert-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;;; init.el ends here
```

### Package Management Setup

**For beginners: package.el with use-package**
```elisp
;; Already included in init.el above
```

**For advanced users: straight.el**
```elisp
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
```

## 2. Terminal Emulator Setup within Emacs

### vterm Installation (High Performance)

```elisp
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/bin/bash")
  (setq vterm-max-scrollback 10000)
  (setq vterm-environment '("TERM=xterm-256color"
                           "COLORTERM=truecolor"))
  
  ;; Performance optimizations
  (setq vterm-timer-delay 0.01)
  (setq vterm-clear-scrollback-when-clearing t)
  
  :bind (("C-c t" . vterm)
         ("C-c T" . vterm-other-window)))
```

**Shell Integration (.bashrc):**
```bash
# vterm shell integration
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
    
    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    PS1=$PS1'\[$(vterm_prompt_end)\]'
fi
```

### eat Installation (Better Integration)

```elisp
(use-package eat
  :ensure t
  :config
  (setq eat-kill-buffer-on-exit t)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :bind (("C-c e" . eat)
         :map eat-mode-map
         ("C-c C-l" . eat-line-mode)
         ("C-c C-e" . eat-emacs-mode)))
```

### Comparison with Ghostty Features

| Ghostty Feature | vterm/eat Equivalent | Notes |
|----------------|---------------------|-------|
| GPU Acceleration | Not available | Use vterm for best software performance |
| Native Tabs | Emacs buffers | Better integration with workflow |
| Ligatures | Font-dependent | Works with JetBrains Mono, Fira Code |
| Directory Tracking | Supported | Both vterm and eat support this |
| Theme Switching | Via Emacs themes | More customizable |

### Environment Configuration

```elisp
;; Sync PATH with shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; direnv integration
(use-package direnv
  :ensure t
  :config
  (direnv-mode)
  (add-to-list 'direnv-non-file-modes 'vterm-mode)
  (add-to-list 'direnv-non-file-modes 'eat-mode))
```

## 3. Claude Code Integration

### Installing claude-code.el

```elisp
;; Using straight.el (recommended for Claude Code)
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" 
             :branch "main" :depth 1)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :config
  (setq claude-code-terminal-backend 'eat)
  (claude-code-mode))

;; Alternative: MCP-focused integration
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))
```

### Authentication Setup

Create `~/.authinfo.gpg`:
```
machine api.anthropic.com login apikey password sk-ant-api03-your-api-key-here
```

Configure in Emacs:
```elisp
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
(setq claude-shell-api-token 
      (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))
```

### Project-Based Sessions

```elisp
;; Each project gets its own Claude instance automatically
;; Buffer naming: *claude-code[project-name]*

;; Create project-specific configuration
;; In project/.dir-locals.el:
((nil . ((claude-code-ide-system-prompt . "You are an expert in React and TypeScript."))))
```

### MCP (Model Context Protocol) Setup

```elisp
;; Enable built-in MCP tools
(claude-code-ide-emacs-tools-setup)

;; Available MCP tools:
;; - xref-find-references: Find symbol references
;; - treesit-info: Get syntax tree information
;; - project-info: Get project information
```

### Keybindings

```elisp
;; Key bindings with C-c c prefix:
;; C-c c c - Start Claude
;; C-c c s - Send command
;; C-c c x - Send command with context
;; C-c c r - Send region
;; C-c c e - Fix error at point
;; C-c c t - Toggle window
```

## 4. GitHub Copilot CLI Integration

### Installation

```bash
# Install Node.js 22+ first if needed
curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash -
sudo apt install nodejs

# Install Copilot CLI
npm install -g @github/copilot
copilot  # Follow authentication
```

### Emacs Integration

```elisp
(defun copilot-cli-explain ()
  "Use Copilot CLI to explain selected code."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun))))
    (async-shell-command
     (format "echo %s | copilot explain" (shell-quote-argument code))
     "*Copilot Output*")))

(defun copilot-cli-fix ()
  "Get fix suggestions for current file."
  (interactive)
  (async-shell-command
   (format "copilot fix %s" (buffer-file-name))
   "*Copilot Fix*"))

(global-set-key (kbd "C-c h e") 'copilot-cli-explain)
(global-set-key (kbd "C-c h f") 'copilot-cli-fix)
```

### Shell Aliases

```elisp
;; Eshell aliases
(defun my-eshell-setup ()
  (eshell/alias "copilot" "async-shell-command copilot $*")
  (eshell/alias "ghex" "copilot explain $*")
  (eshell/alias "ghfix" "copilot fix $*"))

(add-hook 'eshell-mode-hook 'my-eshell-setup)
```

## 5. Google Gemini CLI Integration

### Installation

```bash
# Install Gemini CLI
npm install -g @google/gemini-cli

# Authenticate (free tier: 60 req/min, 1000 req/day)
gemini  # Choose "Login with Google"
```

### Emacs Integration

```elisp
(defun gemini-cli-query (prompt)
  "Send query to Gemini CLI."
  (interactive "sGemini query: ")
  (async-shell-command
   (format "gemini -p %s" (shell-quote-argument prompt))
   "*Gemini Output*"))

(defun gemini-cli-explain-code ()
  "Explain current code with Gemini."
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun))))
    (gemini-cli-query (format "Explain this code:\n\n%s" code))))

(global-set-key (kbd "C-c g q") 'gemini-cli-query)
(global-set-key (kbd "C-c g e") 'gemini-cli-explain-code)
```

### AgentShell Setup

```elisp
(use-package acp
  :straight (:host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell")
  :after acp
  :config
  (setq gemini-acp-server-config
        '(:command "gemini"
          :args ("--experimental-acp")
          :environment (("GEMINI_API_KEY" . "your-key")))))
```

## 6. Unified AI Workflow Configuration

### gptel Setup (General LLM Interface)

```elisp
(use-package gptel
  :ensure t
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-stream t)
  
  ;; Multiple providers
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source)
  
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(llama-3.1-70b-versatile mixtral-8x7b-32768))
  
  :bind (("C-c a c" . gptel)
         ("C-c a s" . gptel-send)
         ("C-c a r" . gptel-rewrite)
         ("C-c a m" . gptel-menu)))
```

### Unified Keybinding Scheme (Hydra)

```elisp
(use-package hydra :ensure t)

(defhydra hydra-ai (:color blue :hint nil)
  "
^AI Operations^          ^Code^                ^Email^
^─────────────^──────────^────^────────────────^─────^──────
_c_: Chat (gptel)        _r_: Refactor         _s_: Summarize
_s_: Send region         _e_: Explain code     _d_: Draft reply  
_m_: Menu               _f_: Fix errors       _t_: Tag emails
_b_: Switch backend     _v_: Review PR        _a_: Auto-respond
"
  ("c" gptel)
  ("s" gptel-send)
  ("m" gptel-menu)
  ("b" my/ai-code-switch-backend)
  ("r" ai-code-refactor)
  ("e" ai-code-explain-code)
  ("f" ai-code-fix-errors)
  ("v" ai-code-review-pr)
  ("s" my/ai-email-summarize)
  ("d" my/ai-email-draft-reply)
  ("t" my/ai-email-smart-tag)
  ("a" my/ai-email-auto-respond)
  ("ESC" nil "quit"))

(global-set-key (kbd "C-c C-a") 'hydra-ai/body)
```

### Buffer Management

```elisp
;; AI buffer management
(defcustom ai-buffer-directory "~/.emacs.d/ai-sessions/"
  "Directory for persistent AI sessions."
  :type 'string)

(defun my/create-ai-buffer (name &optional persistent)
  "Create an AI interaction buffer."
  (let* ((buffer-name (format "*AI-%s*" name))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (when persistent
        (setq buffer-file-name 
              (expand-file-name (concat name ".org") ai-buffer-directory)))
      (org-mode)
      (gptel-mode 1))
    buffer))

(defun my/setup-ai-windows ()
  "Setup optimal window configuration for AI workflows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer (my/create-ai-buffer "main-session" t))
  (window-resize nil (- (/ (window-total-width) 3)) t))
```

## 7. Email Automation Setup (mu4e)

### Installation

```bash
# Install mu4e
sudo apt update
sudo apt install mu4e maildir-utils isync
```

### Configuration

```elisp
(use-package mu4e
  :ensure nil
  :config
  (setq mu4e-maildir "~/Maildir"
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300))

;; AI-powered email functions
(defun my/ai-email-summarize ()
  "Summarize current email using AI."
  (interactive)
  (when (derived-mode-p 'mu4e-view-mode)
    (let ((content (mu4e-message-body-text (mu4e-message-at-point))))
      (gptel-request content 
                    :system "Summarize this email in 2-3 bullet points."
                    :callback (lambda (response)
                               (message "Summary: %s" response))))))

(defun my/ai-email-draft-reply ()
  "Generate AI draft reply."
  (interactive)
  (when (derived-mode-p 'mu4e-view-mode)
    (mu4e-compose-reply)
    (let ((content (mu4e-message-body-text (mu4e-message-at-point))))
      (gptel-request (format "Original: %s" content)
                    :system "Draft a professional reply."
                    :callback (lambda (response)
                               (goto-char (point-max))
                               (insert "\n" response))))))

;; Keybindings
(with-eval-after-load 'mu4e
  (define-key mu4e-view-mode-map (kbd "A s") 'my/ai-email-summarize)
  (define-key mu4e-view-mode-map (kbd "A d") 'my/ai-email-draft-reply))
```

## 8. Performance Optimization for Ubuntu 25.04

### Native Compilation Benefits

Native compilation provides **2-5x speed improvement** for Elisp execution. Verify with:

```elisp
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation: ENABLED")
  (message "Native compilation: DISABLED"))
```

### Memory Management for AI Tools

```elisp
;; Optimize for AI workloads
(setq gc-cons-threshold (* 100 1024 1024)  ; 100MB
      gc-cons-percentage 0.2)

;; Increase process buffer size for AI responses
(setq read-process-output-max (* 4 1024 1024))  ; 4MB

;; Auto-cleanup AI processes
(defun cleanup-ai-processes ()
  "Clean up finished AI processes."
  (dolist (proc (process-list))
    (when (and (string-match-p "ai\\|gpt\\|claude\\|gemini" (process-name proc))
               (memq (process-status proc) '(exit signal)))
      (delete-process proc))))

(run-with-timer 300 300 'cleanup-ai-processes)
```

### Startup Optimization

```elisp
;; Benchmark startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; Daemon mode for instant startup
# Create systemd service: ~/.config/systemd/user/emacs.service
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1)

[Service]
Type=notify
ExecStart=/usr/local/bin/emacs --fg-daemon
Restart=on-failure

[Install]
WantedBy=default.target
```

Enable with:
```bash
systemctl --user enable emacs.service
systemctl --user start emacs.service
alias ec='emacsclient -c -a ""'
```

## 9. Troubleshooting Common Issues

### Ubuntu-Specific Problems

**Font Rendering Issues:**
```bash
# Install better fonts
sudo apt install fonts-firacode ttf-mscorefonts-installer

# Configure in ~/.Xresources
Xft.antialias: 1
Xft.hinting: 1
Xft.rgba: rgb
```

**vterm Compilation Failures:**
```bash
# Fix dependencies
sudo apt install cmake libtool-bin libvterm-dev

# Manual compilation if needed
cd ~/.emacs.d/elpa/vterm-*/
mkdir build && cd build
cmake -DUSE_SYSTEM_LIBVTERM=OFF ..
make
```

### Authentication Problems

**GPG Issues:**
```elisp
;; Fix pinentry in Emacs
(use-package pinentry
  :ensure t
  :config
  (pinentry-start)
  (setq epa-pinentry-mode 'loopback))
```

**SSH Agent:**
```bash
# Add to ~/.bashrc
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
export GPG_TTY=$(tty)
```

### Performance Bottlenecks

Monitor with:
```elisp
;; Profile CPU usage
M-x profiler-start RET cpu RET
;; Use Emacs normally
M-x profiler-stop
M-x profiler-report

;; Check memory
M-: (garbage-collect)
```

## 10. Migration Tips from Ghostty

### Feature Comparison

| Ghostty Feature | Emacs Solution | Adaptation Required |
|----------------|----------------|-------------------|
| GPU Acceleration | Not available | Minimal - vterm is fast enough |
| Native Tabs | Buffer/tab-bar | Learn buffer navigation |
| Quick Terminal | `C-c t` for vterm | Configure keybinding |
| Theme Switching | Emacs themes | More powerful |
| Ligatures | Font-dependent | Use Fira Code or JetBrains Mono |

### Workflow Adaptations

**Terminal Multiplexing:**
```elisp
;; Multi-terminal management
(use-package multi-vterm
  :ensure t
  :bind (("C-c m t" . multi-vterm)
         ("C-c m n" . multi-vterm-next)
         ("C-c m p" . multi-vterm-prev)))
```

**Project-Aware Terminals:**
```elisp
(defun my/vterm-project ()
  "Open vterm in project root."
  (interactive)
  (let* ((project (project-current))
         (root (if project (project-root project) default-directory)))
    (let ((default-directory root))
      (vterm (format "vterm-%s" (file-name-nondirectory 
                                (string-trim-right root "/")))))))

(global-set-key (kbd "C-c p t") 'my/vterm-project)
```

### What to Expect Differently

**Benefits:**
- Unified keybindings across all contexts
- Superior text manipulation in terminals
- Integrated development environment
- Extensible with thousands of packages

**Trade-offs:**
- No GPU acceleration (but vterm is fast enough)
- Steeper learning curve initially
- More complex configuration

## Complete Starter Configuration

Save this as `~/.emacs.d/init.el` for a complete working setup:

```elisp
;;; init.el --- Complete AI-powered Emacs configuration -*- lexical-binding: t -*-

;; Performance optimizations
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.2
      read-process-output-max (* 4 1024 1024))

;; Package management
(require 'package)
(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Terminal emulator
(use-package vterm
  :bind (("C-c t" . vterm))
  :config
  (setq vterm-max-scrollback 10000))

;; AI tools
(use-package gptel
  :bind (("C-c a c" . gptel)
         ("C-c a s" . gptel-send))
  :config
  (setq gptel-stream t))

;; Theme
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

;; Completion
(use-package vertico
  :config (vertico-mode))

(use-package marginalia
  :config (marginalia-mode))

;; Basic settings
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode 1)
(save-place-mode 1)

(message "Welcome to your AI-powered Emacs!")
;;; init.el ends here
```

This configuration provides a solid foundation for your Emacs journey, with all the AI tools integrated and ready to use. As you become more comfortable, you can gradually add more features and customizations from this guide.