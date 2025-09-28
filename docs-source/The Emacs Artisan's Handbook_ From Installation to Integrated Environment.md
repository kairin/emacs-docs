# **The Emacs Artisan's Handbook: From Installation to Integrated Environment**

### **A Tailored Guide for the Ubuntu Artisan**

Welcome. This handbook has been adapted to serve as your personalized guide to building a sophisticated Emacs environment from the ground up. Recognizing your context as a user of Ubuntu 25.04, the Ghostty terminal, and the Zsh shell, this guide will focus on the most direct and powerful path for your specific setup. We will bypass irrelevant platform details and concentrate on achieving your stated goals: integrating cutting-edge AI development tools like Claude Code, Gemini CLI, and Copilot CLI, and transforming Emacs into a command center for managing and automating your Outlook.com email. This journey is about crafting a tool that is not just powerful, but perfectly attuned to your workflow.

## **Part I: The Foundation \- Installation and Initial Sanity**

This first part of the handbook is dedicated to overcoming the most significant initial barrier to entry for any aspiring Emacs user: achieving a stable, functional, and performant installation. The following chapters will address the platform-specific challenges that often frustrate newcomers and guide the process of establishing a clean, minimal baseline configuration upon which a sophisticated personal environment can be built.

### **Chapter 1: Before You Begin: The Emacs Philosophy**

To effectively wield Emacs, one must first adjust their mental model. It is not merely a text editor in the conventional sense; it is a malleable computing environment, an interpreter for the Lisp programming language that happens to ship with powerful text-editing capabilities.1 Understanding this core philosophy is the prerequisite to unlocking its true potential and appreciating the purpose behind its configuration-as-code approach.

#### **Emacs as a Lisp Machine**

At its heart, GNU Emacs is an environment for executing Emacs Lisp (Elisp) code. Nearly every function, from inserting a character to managing a version control repository, is an Elisp function that can be inspected, modified, or replaced by the user.3 This is the source of its unparalleled extensibility. Unlike applications configured through graphical user interfaces or simple key-value files, Emacs is configured by writing code. This approach grants the user the power to alter any aspect of its behavior, creating a tool that is uniquely tailored to their specific needs and workflows.

#### **The Building Blocks: Buffers, Windows, and Frames**

A clear understanding of Emacs's core architectural concepts is essential for navigating its interface and documentation. These three components—buffers, windows, and frames—form the foundation of the user experience:

* **Buffer:** A buffer is an in-memory object that holds text. When a file is opened, its contents are read into a buffer. However, many buffers do not correspond to files, such as the \*scratch\* buffer for temporary notes or the \*Messages\* buffer for logs.3 The text itself lives in the buffer.  
* **Window:** A window is a viewport onto a buffer. A single buffer can be displayed in multiple windows simultaneously, perhaps showing different parts of the same file. Conversely, a single window can be used to view different buffers over time.3  
* **Frame:** A frame is what the host operating system considers a window. An Emacs session can have multiple frames, which can be useful on multi-monitor setups.

Mastering the commands to manipulate these three entities is a key step toward proficiency.

#### **The Heart of Your Configuration: init.el**

The central script that Emacs executes upon startup is the initialization file, commonly referred to as init.el. This file is the home for all user-defined configurations, from setting simple variables to loading complex packages. Emacs searches for this file in a specific order of precedence, but the modern, conventional location is \~/.emacs.d/init.el.1 Older configurations might use

\~/.emacs or \~/.emacs.el, but new configurations should be built around the \~/.emacs.d/ directory structure.6

#### **The Learning Curve as an Investment**

The learning curve for Emacs is notoriously steep, a fact that should be acknowledged rather than dismissed.4 However, this initial difficulty is a front-loaded investment. The time spent learning the fundamentals of Elisp and the Emacs environment pays compounding dividends in productivity over a career. Unlike tools that are simple to learn but offer a low ceiling for mastery, Emacs provides a nearly limitless potential for customization and workflow optimization. The journey from a novice user to an "Emacs Artisan" is one of transforming a general-purpose tool into a bespoke instrument perfectly honed for one's craft.

### **Chapter 2: Installation on Ubuntu 25.04**

As an Ubuntu 25.04 user, you are in the ideal environment for Emacs and have several excellent installation options. This chapter provides a focused guide to getting a modern version of Emacs running on your system and ensuring it integrates perfectly with your preferred shell and terminal.

#### **Installing via PPA (Recommended for Beginners)**

Using a Personal Package Archive (PPA) is a straightforward way to install a recent, pre-compiled version of Emacs that is more up-to-date than the one in Ubuntu's default repositories. This method balances modernity with ease of installation.68

1. **Add the PPA:** Open your terminal and add the recommended Emacs PPA:  
   Bash  
   sudo add-apt-repository ppa:ubuntuhandbook1/emacs

   68  
2. **Install Emacs:** Update your package list and install the emacs package. You can also choose a specific graphical toolkit version, such as emacs-pgtk for native Wayland support.68  
   Bash  
   sudo apt update  
   sudo apt install emacs

#### **Installing via Snap**

Another simple method is to use a Snap package, which bundles the application with its dependencies. This is often a quick way to get the latest stable release.69

Bash

sudo snap install emacs \--classic

68

The \--classic flag is necessary to allow the application to access files outside of its sandbox, which is essential for a development tool.68

#### **The Artisan's Path: Compiling from Source**

For maximum control and access to the latest performance features like native compilation, compiling from source is the ultimate path.

1. **Install Build Dependencies:** Ubuntu makes this easy. The apt build-dep command automatically installs all the libraries required to build the version of Emacs in the main repository.8  
   Bash  
   sudo apt build-dep emacs

2. **Download and Compile:** Download the latest source code from the GNU FTP server, extract it, and run the standard compilation commands. The \--with-native-compilation flag is highly recommended for a significant speed boost.7

./configure \--with-native-compilation \--with-json \--with-tree-sitter  
make \-j$(nproc)  
sudo make install  
\`\`\`

8

#### **Terminal and Shell Integration**

You've indicated a preference for the Ghostty terminal and Zsh with Oh My Zsh.73 While Emacs has excellent built-in terminal emulators, its graphical interface offers the richest experience. The most critical step to ensure seamless integration between the GUI Emacs and your command-line tools is to make sure Emacs inherits your shell's environment variables.

A GUI application launched from the desktop environment does not automatically read your \~/.zshrc or \~/.zshenv files. This means Emacs won't know the PATH you've configured, and it won't be able to find programs installed by apt, pip, or other tools.

The definitive solution is the exec-path-from-shell package. It should be one of the very first packages you configure in your init.el. This package ensures that Emacs's internal exec-path mirrors your Zsh PATH, allowing it to find all your command-line utilities.74

Code snippet

;; In init.el  
(use-package exec-path-from-shell  
  :ensure t  
  :config  
  (exec-path-from-shell-initialize))

75

### **Chapter 3: First Configuration: Essential Defaults and Performance Tuning**

With a stable Emacs installation in place, the next step is to create a minimal, stable, and fast init.el that will serve as the foundation for all future customizations. This involves setting up both an early-init.el for pre-initialization performance tuning and an init.el for basic usability enhancements.

#### **Creating init.el and early-init.el**

Modern Emacs configurations are best split into two files. The early-init.el file is loaded before the package system and GUI are initialized, making it the ideal location for settings that affect startup performance and the initial appearance of the Emacs frame.10 The main

init.el file is loaded afterward and contains the bulk of the configuration. Both files reside in the \~/.emacs.d/ directory.

The code placed in early-init.el should be minimal and focused on tasks that must happen before package initialization.

* **Garbage Collection Tuning:** The single most impactful performance tweak is to adjust the garbage collection threshold. The default value is extremely low for modern hardware (around 800 kB) and can cause frequent, noticeable pauses during startup as Emacs loads packages.7 The optimal strategy is to set a very high threshold at the beginning of  
  early-init.el and then add a hook to restore it to a more reasonable value after initialization is complete.  
  Code snippet  
  ;; In early-init.el  
  (setq gc-cons-threshold (\* 100 1024 1024)) ;; 100 MB

  Code snippet  
  ;; In init.el  
  (add-hook 'emacs-startup-hook  
            (lambda () (setq gc-cons-threshold (\* 2 1024 1024)))) ;; 2 MB

  12  
* **Disable UI Elements Early:** To prevent graphical elements like the toolbar, menu bar, and scroll bars from ever being drawn, they should be disabled in early-init.el. This provides a small but measurable startup speed improvement.  
  Code snippet  
  ;; In early-init.el  
  (tool-bar-mode \-1)  
  (menu-bar-mode \-1)  
  (scroll-bar-mode \-1)

  11

#### **Basic Sanity Settings in init.el**

The init.el file should begin with a curated list of settings that align Emacs with modern editor conventions and improve its quality of life.

* **Disable Annoyances:**  
  Code snippet  
  ;; Disable the startup splash screen  
  (setq inhibit-startup-screen t)

  ;; Use y/n prompts instead of yes/no  
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Disable the audible/visual bell  
  (setq ring-bell-function 'ignore)

  7  
* **Set Sensible Defaults:**  
  Code snippet  
  ;; Prefer UTF-8 for encoding  
  (set-charset-priority 'unicode)  
  (setq locale-coding-system 'utf-8)  
  (set-terminal-coding-system 'utf-8-unix)  
  (set-keyboard-coding-system 'utf-8)  
  (set-selection-coding-system 'utf-8)  
  (prefer-coding-system 'utf-8)

  ;; Use spaces instead of tabs for indentation  
  (setq-default indent-tabs-mode nil)

  ;; Save backups to a dedicated directory  
  (setq backup-directory-alist \`((".". "\~/.emacs.d/backups")))

  11

#### **Managing Customizations**

Emacs provides a powerful interactive customization interface, accessible via M-x customize. When a user saves changes through this interface, Emacs writes the corresponding Elisp code to a file. By default, this code is appended directly to init.el, which can make the hand-written configuration messy and difficult to manage with version control.

The best practice is to redirect these automatic customizations to a separate file. This keeps the main init.el clean and dedicated to manually curated code.

Code snippet

;; In init.el  
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))  
(load custom-file :noerror)

12

This configuration tells Emacs to save all changes made via the customize interface to \~/.emacs.d/custom.el and to load that file if it exists. This file can then be added to the version control system's ignore file (e.g., .gitignore).

## **Part II: The Toolkit \- Package and Configuration Management**

Having established a stable installation and a sane baseline, this part transitions to the core of the artisan's craft: building a powerful, extensible, and maintainable configuration. The following chapters explore the philosophies and tools that define a modern Emacs workflow, focusing on declarative package management and a structured approach to organizing the configuration itself.

### **Chapter 4: The Package Management Ecosystem**

Emacs's power is magnified by its vast ecosystem of third-party packages. Effectively managing these packages is crucial for building a robust and reproducible environment. This chapter provides a clear understanding of how Emacs is extended, compares the available tools, and recommends a strategy that prioritizes declarative configuration and reproducibility.

#### **The Foundation: package.el and Package Archives**

The built-in package manager for Emacs is package.el.1 It is the foundation upon which more sophisticated tools are built. To use it, one must first configure the

package-archives variable to point to the repositories from which packages will be downloaded. While GNU ELPA is configured by default, the most essential repository to add is **MELPA** (Milkypostman's Emacs Lisp Package Archive), which is the largest and most up-to-date collection of Emacs packages.17

The initial setup in init.el should include the following:

Code snippet

(require 'package)  
(add-to-list 'package-archives '("melpa". "https://melpa.org/packages/"))  
(package-initialize)

17

While package.el provides an interactive interface via M-x package-list-packages for browsing and installing packages, this manual approach is not ideal for creating a configuration that can be easily version-controlled and deployed on multiple machines.1

#### **Declarative Configuration with use-package**

The use-package macro is the de facto standard for modern Emacs configuration. It solves the problem of init.el degrading into "spaghetti code" by providing a declarative syntax to group all configuration related to a single package—including its installation, keybindings, custom variables, and hooks—into a single, coherent block.14

* **Core Keywords and Concepts:**  
  * :ensure: This keyword is the bridge to package.el. Setting :ensure t instructs use-package to automatically download and install the package from the configured archives if it is not already present.21  
  * :init vs. :config: This is a crucial distinction for both correctness and performance. Code in an :init block is executed *before* the package is loaded. This is for settings that must exist prior to the package's own code running. In contrast, code in a :config block is executed *after* the package has been loaded. The vast majority of configuration belongs in the :config block.19  
  * **Lazy Loading for Performance:** One of the greatest benefits of use-package is its ability to facilitate lazy loading, which dramatically reduces Emacs's startup time. Instead of loading every package when Emacs starts, use-package can defer loading until a package's functionality is actually invoked. This is achieved automatically through keywords like:  
    * :bind: Defines keybindings. The package is not loaded until one of these keybindings is pressed for the first time.  
    * :mode: Associates a package with a file extension (e.g., loading python-mode when a .py file is opened).  
    * :hook: Adds a function from the package to a hook. The package is loaded when that hook is run.  
      This lazy-loading mechanism is a significant improvement over a naive configuration that uses (require 'package-name) for everything, which would force all packages to be loaded at startup.22  
* **Bootstrapping use-package:** Since use-package is itself a package, the init.el file must contain a small snippet to ensure it is installed on a fresh system before it is used to configure other packages.  
  Code snippet  
  (unless (package-installed-p 'use-package)  
    (package-refresh-contents)  
    (package-install 'use-package))  
  (require 'use-package)

  6

#### **Reproducible Environments with straight.el**

For the artisan seeking the highest degree of control and reproducibility, straight.el represents the next evolution in package management. It is a package manager that installs packages directly from their source Git repositories, rather than from a pre-packaged archive like MELPA.26

* **The Rationale for straight.el:**  
  * **Reproducibility:** The primary advantage of straight.el is its ability to generate a lockfile that pins every package and its dependencies to a specific Git commit. This guarantees that the Emacs environment is 100% reproducible across different machines or at different points in time, eliminating the "it works on my machine" problem that can arise when MELPA packages are updated.26  
  * **Flexibility:** It becomes trivial to use personal forks or specific development branches of packages, a task that is cumbersome with package.el.26  
  * **Upstream Contributions:** Since the local installation is a full Git repository, it is much easier to make changes, test them, and contribute fixes back to the package's original author.26  
* **Integration with use-package:** straight.el is designed to integrate seamlessly with use-package. The configuration involves replacing :ensure t with :straight t. For a more streamlined setup, one can configure use-package to use straight.el by default.  
  Code snippet  
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

  ;; Configure use-package to use straight.el by default  
  (straight-use-package 'use-package)  
  (setq straight-use-package-by-default t)

  26

The combination of straight.el for Git-native, reproducible package management and use-package for clean, declarative configuration represents the gold standard for the modern Emacs artisan. It provides unparalleled control, stability, and maintainability.

### **Chapter 5: Architecting Your Configuration**

As an Emacs configuration grows, a single init.el file can become an unmanageable monolith. This chapter discusses strategies for organizing the configuration files themselves to ensure long-term maintainability and clarity. It also addresses the fundamental choice every new user faces: whether to build a configuration from scratch or adopt a pre-packaged "distribution."

#### **Structuring Your .emacs.d**

There are several effective strategies for structuring an Emacs configuration, ranging from simple to highly sophisticated.

* **The Single File:** For a beginner, keeping all configuration within \~/.emacs.d/init.el is the simplest approach. It is perfectly viable for small configurations but becomes difficult to navigate as the number of packages and custom functions grows.20  
* **Modular Files:** A more scalable approach is to break the configuration into multiple .el files, each focused on a specific area of functionality (e.g., init-ui.el, init-editing.el, init-org.el). These files are then loaded from the main init.el using the load-file function. This modularity improves organization and makes it easier to locate specific settings.28  
* **Literate Configuration with Org Mode:** This is the most advanced and arguably the most "Emacs-like" method. The entire configuration is written within a single Org Mode file (e.g., config.org). This file combines executable Elisp code blocks with extensive prose, explanations, diagrams, and links. The configuration becomes a self-documenting, human-readable document. Using Org Babel's "tangle" feature, the code blocks are extracted from the .org file to generate the actual .el files that Emacs loads on startup.7 This approach encourages thoughtful, well-documented configuration and is the hallmark of many expert Emacs users.

#### **Synchronizing Your Configuration Across Devices**

Since you use Emacs across various devices, ensuring a consistent experience is paramount. The most robust method for this is to place your entire \~/.emacs.d directory under version control with Git. This turns your configuration into a portable, reproducible project.

You can simply initialize a Git repository inside \~/.emacs.d, create a .gitignore file to exclude transient files (like backups, history, and package directories if you're not using a lockfile), and push it to a private remote repository. On a new machine, you just need to clone this repository into \~/.emacs.d, and upon starting Emacs, your package manager (use-package with :ensure or straight.el) will automatically install all the necessary packages, perfectly recreating your environment. For managing your entire suite of configuration files (dotfiles), tools like GNU Stow can be used to manage symbolic links from a central repository to their correct locations in your home directory.77

#### **The Great Debate: Vanilla vs. Distributions**

A fundamental decision for any user is whether to build a configuration from scratch ("vanilla") or to adopt a pre-configured distribution. Distributions like Spacemacs and Doom Emacs offer a curated, out-of-the-box experience but trade some control and transparency for convenience.

* **Spacemacs:** A community-driven configuration that aims to be highly user-friendly and discoverable. Its core philosophy is built around mnemonic keybindings, accessed via the Space bar leader key, and a "layer" system. Layers are bundles of packages and configuration for a specific purpose, such as the python layer or the git layer, which can be enabled or disabled easily in the user's .spacemacs file.31 While powerful and well-suited for users coming from Vim, it can feel abstract and has historically had slower startup times than more minimalist setups.33  
* **Doom Emacs:** A more performance-oriented and opinionated framework, also heavily centered on Vim-style modal editing (evil-mode).34 Doom is renowned for its fast startup times, achieved through extensive lazy-loading and other optimizations. It uses a command-line utility,  
  bin/doom, for managing the configuration, such as syncing packages and upgrading.35 It is often considered less abstract than Spacemacs but can be more opaque to debug when issues arise, as it modifies some of Emacs's core behaviors.36  
* **Comparative Analysis:** The choice between these approaches involves a series of trade-offs. The following table summarizes the key differences to aid in this decision.

| Factor | Vanilla Emacs | Spacemacs | Doom Emacs |
| :---- | :---- | :---- | :---- |
| **Control & Transparency** | Absolute. The user builds and understands every line of code. | Moderate. Layers provide abstraction; deep customization can require fighting the framework. | High. Less abstract than Spacemacs, but still an opinionated framework. |
| **Initial Learning Curve** | Very high. Requires learning Elisp and Emacs concepts from the ground up. | Moderate. Mnemonic keybindings and layers aid discoverability. | High. Fast-paced and assumes familiarity with Vim and command-line tools. |
| **Startup Performance** | User-dependent. Can be made extremely fast with careful lazy-loading. | Generally slower due to the large number of packages and layers loaded. | Excellent. A primary design goal, typically the fastest of the three. |
| **Out-of-the-Box Experience** | Minimal. A blank slate requiring significant effort to become a modern IDE. | Rich. A full-featured IDE for many languages once the appropriate layers are enabled. | Rich. Provides a polished, IDE-like experience with a focus on performance. |
| **Keybinding Philosophy** | Default Emacs keybindings. The user must build their own scheme. | Vim-centric (Evil mode) by default, with a comprehensive mnemonic leader-key system. | Vim-centric (Evil mode) by default, with a highly optimized leader-key system. |
| **Configuration Model** | Manual, via .el or .org files. Can be structured in any way the user desires. | Declarative via layers in a central .spacemacs file. | Declarative via modules in a config.el file, managed by a CLI tool. |
| **Community & Support** | The entire Emacs community. Solutions are general and widely applicable. | Strong, dedicated community, but support is often specific to the Spacemacs ecosystem. | Strong, active community, particularly on Discord. Support is Doom-specific. |

While distributions are an excellent way to get a powerful Emacs environment up and running quickly, the "Artisan" path of building a vanilla configuration from scratch provides a deeper understanding, ultimate control, and a tool that is perfectly and uniquely tailored to its user. This handbook is dedicated to guiding that path.

## **Part III: Crafting the Experience \- Core Enhancements**

With a robust installation and a clear configuration strategy, the focus now shifts to installing and configuring the packages that form the bedrock of a modern, efficient Emacs environment. These core enhancements are largely domain-agnostic and will dramatically improve the user experience for any professional workflow.

### **Chapter 6: The Modern Minibuffer: A Deep Dive into Completion Frameworks**

The minibuffer is Emacs's command line, used for everything from executing commands with M-x to finding files and switching buffers. Enhancing the completion experience within the minibuffer is one of the most significant productivity boosts available. The Emacs ecosystem has seen a significant evolution in this area, moving from monolithic frameworks to a more modular and composable set of tools.

This shift toward modularity represents a deeper philosophical alignment with the core principles of Emacs itself. The older, monolithic frameworks like Helm and Ivy sought to replace Emacs's built-in completion system (completing-read) with their own custom APIs. While powerful, this created isolated ecosystems. The modern stack, in contrast, is built *on top of* the native completion system. Each component—Vertico for the UI, Marginalia for annotations, Orderless for filtering—is a smaller, focused package that enhances one aspect of the existing infrastructure.38 This approach is more robust, less intrusive, and allows for greater flexibility, as users can mix and match components to suit their preferences. It is a return to the Unix philosophy of small tools that do one thing well, composed to create a powerful whole.

#### **Implementing the Vertico Stack**

The recommended modern setup consists of a suite of packages that work together seamlessly. The following use-package declarations provide a complete, high-performance completion system.

* **Vertico:** This package provides the core user interface, replacing the default horizontal completion with a clean, minimalistic, and highly responsive vertical display in the minibuffer.  
  Code snippet  
  (use-package vertico  
    :init  
    (vertico-mode))

  38  
* **Marginalia:** This package enriches the Vertico display by adding helpful annotations to completion candidates in the margins. For example, when completing commands with M-x, it will show the associated keybinding and a one-line summary of the function's purpose. When completing files, it shows permissions and modification times.  
  Code snippet  
  (use-package marginalia  
    :after vertico  
    :init  
    (marginalia-mode))

  38  
* **Orderless:** A powerful and intuitive completion style that allows filtering candidates by entering space-separated tokens in any order. For instance, to find the file \~/.emacs.d/init.el, one could simply type init el.emacs into the find-file prompt.  
  Code snippet  
  (use-package orderless  
    :init  
    (setq completion-styles '(orderless basic)  
          completion-category-overrides '((file (styles basic partial-completion)))))

  38  
* **Consult:** This package provides a collection of enhanced commands that leverage the modern completion system. It offers superior alternatives to many built-in commands, such as consult-grep (or consult-ripgrep) for project-wide search, consult-buffer for switching buffers, and consult-imenu for navigating symbols in a file.  
  Code snippet  
  (use-package consult)

  40  
* **Embark:** Embark provides a context-aware action system that operates on the completion candidates. After filtering a list of candidates in the minibuffer (e.g., a list of files from consult-find), one can press a key to bring up a menu of possible actions to perform on the selected candidate, such as deleting it, renaming it, or viewing its Git history. This creates a powerful and fluid "filter-then-act" workflow.  
  Code snippet  
  (use-package embark  
    :bind  
    (("C-.". embark-act)  
     ("C-;". embark-dwim)  
     ("C-h B". embark-bindings)))

  39

### **Chapter 7: The Ultimate Git Porcelain: Mastering Magit**

Magit is widely considered one of Emacs's "killer applications" and is arguably the most efficient and powerful interface to Git available on any platform.14 Its core philosophy is to be a "Git porcelain," a user interface where every piece of visible information is actionable.42 It does not hide Git's concepts but rather exposes them through an intuitive, text-based interface that is faster and more ergonomic than both the command line and traditional GUIs.

The use of Magit naturally encourages better version control habits. The standard Git command-line interface for interactive staging (git add \--patch) is cumbersome, which often leads developers to create large, messy commits containing unrelated changes.44 Magit makes the process of staging individual lines, hunks, or files trivial—often requiring only a single keystroke. By dramatically lowering the friction of this core workflow, Magit empowers and encourages users to craft a clean, logical, and atomic commit history, thereby making them more effective version control practitioners.45

#### **Installation and Basic Workflow**

Magit is best installed via use-package, with a global keybinding set for its main entry point, the status buffer.

Code snippet

(use-package magit  
  :bind (("C-x g". magit-status)))

46

* **The Status Buffer:** Invoking magit-status (C-x g) opens the main Magit interface, which provides a comprehensive overview of the current repository's state. This buffer is divided into collapsible sections, including unstaged changes, staged changes, the local branch's relationship to its upstream branch, recent commits, and stashes.43  
* **Staging and Committing:** The fundamental workflow is remarkably efficient:  
  1. Navigate to a file or a specific "hunk" (a block of changes) in the "Unstaged changes" section.  
  2. Press s to **stage** that item. It will instantly move to the "Staged changes" section.  
  3. Press u to **unstage** an item.  
  4. Press c to open the commit popup, then c again to open the commit message buffer.  
  5. Write the commit message, and press C-c C-c to finalize the commit.  
  6. Press P to open the push popup, then p to push the changes to the upstream remote.  
     This ability to stage changes at the hunk or even the single-line level with ease is Magit's most celebrated feature.44

#### **Advanced Workflows**

Magit excels at complex Git operations that are often intimidating on the command line.

* **Interactive Rebase:** Initiating an interactive rebase (r then i) presents a clean, editable buffer where commits can be reordered, squashed, edited, or dropped simply by changing text labels and reordering lines.  
* **Blame and Time Machine:** From any buffer, M-x magit-blame will annotate each line with the commit and author that last changed it. For an even more powerful historical view, the git-timemachine package allows the user to step backward and forward through the entire commit history of a file, viewing its state at each point in time.14

### **Chapter 8: Essential Utilities for a Modern Workflow**

Beyond the major systems for completion and version control, a set of general-purpose packages provides essential quality-of-life improvements across all domains.

#### **Project Management with projectile**

Projectile is a project interaction library for Emacs. It automatically detects project roots (typically identified by the presence of a .git directory or other version control markers) and provides a suite of commands for operating on the project as a whole.17

Code snippet

(use-package projectile  
  :init  
  (projectile-mode \+1)  
  :bind-keymap  
  ("C-c p". projectile-command-map))

17

This enables projectile-mode globally and binds its command map to the C-c p prefix. Key commands include:

* C-c p f: Find a file anywhere in the current project (projectile-find-file).  
* C-c p s g: Search (grep) for a string across all files in the project (projectile-ripgrep).  
* C-c p b: Switch to a buffer associated with the current project.

#### **In-Buffer Completion with corfu**

Distinct from the minibuffer completion provided by Vertico, corfu provides an in-buffer completion-at-point system. It displays a small, unobtrusive popup with completion suggestions as one types code or text.40 Corfu is modern and lightweight, building directly on Emacs's native

completion-at-point functionality. It integrates seamlessly with language servers (via eglot or lsp-mode) to provide intelligent, context-aware code completions.40

Code snippet

(use-package corfu  
  :init  
  (global-corfu-mode))

49

#### **Discoverability with which-key**

For both beginners and experts, which-key is an indispensable tool for learning and navigating Emacs's rich keybinding landscape. After a prefix key is pressed (e.g., C-x), which-key displays a popup showing all of the available keybindings that follow that prefix, along with their descriptions. This dramatically improves discoverability and reduces the cognitive load of memorizing hundreds of key combinations.14

Code snippet

(use-package which-key  
  :init  
  (which-key-mode))

14

## **Part IV: Specialization \- Tailoring Emacs for Your Domain**

With a solid, modern foundation in place, this final part provides three distinct blueprints for configuring Emacs for specific professional workflows. Each chapter builds upon the core enhancements from Part III, adding the specialized packages and configurations necessary to transform Emacs into a world-class tool for software development, academic research, or data science.

### **Chapter 9: The Software Developer's Cockpit**

This chapter details the steps to transform Emacs into a modern, full-featured Integrated Development Environment (IDE), leveraging the Language Server Protocol for intelligent code analysis and a dedicated Debug Adapter Protocol client for a visual debugging experience.

#### **The Language Server Protocol (LSP)**

The Language Server Protocol (LSP) is a technology that standardizes communication between development tools and language-specific servers. An LSP server analyzes the source code and provides IDE features like intelligent code completion, go-to-definition, find-references, and real-time diagnostics. The editor acts as a client, requesting this information from the server.14

Emacs has two primary LSP clients: lsp-mode and eglot.

* **lsp-mode:** The more mature and feature-rich of the two, offering extensive customization and integration with a wide array of UI packages. However, it can be more complex to configure.51  
* **eglot (Emacs Polyglot):** A more recent and minimalist client that is now built into Emacs as of version 29\. It aims for simplicity and a more "Emacs-native" feel, integrating with built-in features wherever possible. For its simplicity and official status, eglot is the recommended choice for a new configuration.52

#### **On-the-fly Diagnostics with flycheck**

flycheck is a modern on-the-fly syntax checking extension. It integrates with compilers, linters, and LSP servers to highlight errors and warnings directly in the buffer as code is being written.14 When used with an LSP client like

eglot, diagnostics provided by the language server are automatically displayed via flycheck.

#### **Debugging with dap-mode**

dap-mode implements the Debug Adapter Protocol (DAP), providing a full-featured, visual debugging experience within Emacs. It allows users to set breakpoints, step through code (step in, step over, step out), inspect variables, view call stacks, and interact with a debug console, rivaling the debugging capabilities of traditional IDEs.51

#### **Case Study: A Complete Python Development Environment**

The following use-package declarations and setup instructions demonstrate how to combine these components into a cohesive Python IDE.

1. **Install the Language Server and Linters:** These are external tools that must be installed via pip.  
   Bash  
   pip install 'python-lsp-server\[all\]' flake8 black

   40  
2. **Configure Emacs Packages:** The init.el should contain configurations for eglot, flycheck, and other development utilities.  
   Code snippet  
   ;; Enable eglot for python-mode. It will automatically find and start pylsp.  
   (use-package eglot  
     :hook (python-mode. eglot-ensure))

   ;; Asynchronous code formatting on save  
   (use-package apheleia  
     :init  
     (apheleia-global-mode \+1)  
     :config  
     (setf (alist-get 'black apheleia-formatters)  
           '("black" "-")))

   ;; Manage Python virtual environments  
   (use-package pyvenv  
     :config  
     (pyvenv-mode \+1))

   ;; Code snippet expansion  
   (use-package yasnippet  
     :init  
     (yas-global-mode 1))

   40

With this configuration, opening a Python file will automatically start the pylsp language server, providing intelligent completion via corfu. flycheck will display flake8 linting errors in real-time. When the file is saved, apheleia will run the black formatter asynchronously to ensure consistent code style. The pyvenv package allows for easy switching between virtual environments with M-x pyvenv-workon, ensuring that the correct interpreter and dependencies are used for each project.

### **Chapter 10: The Researcher's Digital Atelier**

For academic writers and researchers, Emacs can be transformed into a powerful digital atelier—an integrated environment for managing bibliographic references, taking structured notes, and authoring complex documents for publication. This workflow is centered on the unparalleled capabilities of Org Mode.

#### **The Foundation: Org Mode**

Org Mode is one of Emacs's premier "killer applications." It is a major mode for outlining, note-taking, project planning, and, most importantly for this context, authoring documents with a simple yet powerful plain-text syntax.14 Its ability to export to a multitude of formats, including LaTeX/PDF, HTML, and ODT, makes it an ideal system for scholarly writing.57

#### **Personal Knowledge Management (PKM): org-roam vs. denote**

A crucial component of modern research is a system for personal knowledge management. In the Emacs ecosystem, two primary contenders have emerged: org-roam and denote. The choice between them reflects a deeper philosophical divide in note-taking methodology.

* **org-roam:** Implements a Zettelkasten-like system by building a SQLite database of all notes and their links. This allows for powerful features like fast backlink discovery and graph visualization. However, it introduces an external dependency (the database) and a layer of abstraction over the plain-text files.58  
* **denote:** A simpler, more robust system that eschews a database. It relies on a consistent and descriptive file-naming convention (YYYYMMDDTHHMMSS--Title\_of\_note.org) to manage notes and their relationships. Links are standard Org links, and discovery is handled by Emacs's powerful file-finding and search tools. This approach is more transparent, portable, and less prone to corruption.60

The philosophy of denote—relying on simple, composable, plain-text principles—aligns more closely with the "from scratch" artisan ethos. It is therefore the recommended choice for this workflow.

#### **The Academic Writing Workflow: citar, denote, and Org Export**

This workflow seamlessly integrates bibliography management, note-taking, and document authoring.

1. **Bibliography Management with citar:** citar is the modern Emacs package for interacting with a bibliographic database (typically a .bib BibTeX file or a CSL JSON file, often managed by an external tool like Zotero).60  
   Code snippet  
   (use-package citar  
     :config  
     (setq citar-bibliography '("\~/path/to/your/bibliography.bib")))

2. **Connecting References to Notes with citar-denote:** The citar-denote package provides the crucial link between the bibliographic database and the note-taking system. With the cursor on a reference in the citar interface, a single command (citar-create-note) creates a new denote note. This note is automatically titled based on the reference, tagged appropriately, and linked back to the original bibliographic entry, creating a "literature note".61  
   Code snippet  
   (use-package citar-denote  
     :after (citar denote)  
     :config  
     (citar-denote-mode \+1))

3. **Writing and Citing:** The authoring process takes place in a standard Org Mode file. When a reference needs to be cited, the citar-insert-citation command is used. This brings up the citar interface to select a reference and inserts a standard Org citation link into the document (e.g., \`\`).64  
4. **Exporting:** Org Mode's powerful export engine handles the final step. When the document is exported to PDF (via LaTeX), Org automatically processes all the cite: links, formats them according to a specified citation style (CSL), and generates a complete, formatted bibliography at the end of the document. The result is a publication-quality academic paper produced from a single plain-text source file.

### **Chapter 11: The Data Scientist's Workbench**

Emacs, configured correctly, can serve as a powerful and interactive environment for data analysis, visualization, and reproducible research, offering a compelling alternative to tools like Jupyter Notebooks and RStudio. The key to this workflow is Org Babel, Emacs's engine for literate programming.

#### **Literate Programming with Org Babel**

Org Babel allows for the embedding of executable source code blocks from a wide variety of programming languages—including Python, R, SQL, and shell scripts—directly within an Org document.65 These code blocks can be executed interactively, and their results (be it text output, data tables, or graphical plots) can be captured and inserted directly into the document. This creates a single, reproducible artifact that seamlessly weaves together narrative, code, and results, which is the cornerstone of reproducible research.67

#### **Interactive Development Environment**

* **R with ESS (Emacs Speaks Statistics):** For R users, the ESS package provides a mature and feature-rich development environment that is on par with, and in some ways superior to, RStudio. It offers intelligent code completion, integration with the R help system, and powerful tools for sending code from a source file to an interactive R process.57  
* **Python:** Building on the software development setup from Chapter 9, the data science workflow emphasizes interactive execution. Commands to send the current line, a selected region, or an entire function to an interactive IPython REPL are essential for exploratory data analysis.65  
* **SQL:** Emacs's built-in sql-mode can be configured to connect directly to various databases. This allows a data scientist to write and execute SQL queries interactively from within Emacs, with the results appearing in a separate buffer, which can then be easily integrated into an Org document.65

#### **Replacing Jupyter with Org Mode: A Workflow Example**

The following workflow demonstrates how Org Babel can replicate and improve upon the functionality of a Jupyter Notebook.

1. **Load Data:** An Org document is created. The first code block is a Python block that uses the pandas library to load a dataset into a DataFrame.  
   Code snippet  
   \#+begin\_src python :results output :exports both  
   import pandas as pd  
   df \= pd.read\_csv('data.csv')  
   print(df.head())  
   \#+end\_src

2. **Explore Data:** Executing this block (C-c C-c) will run the Python code and insert the output (the first five rows of the DataFrame) directly below the block in the Org document.  
3. **Visualize Data:** A second code block uses matplotlib or seaborn to create a plot. The block's header arguments are configured to save the plot to a file and then display that file inline within the Org document.  
   Code snippet  
   \#+begin\_src python :results file :file "plot.png" :exports both  
   import matplotlib.pyplot as plt  
   import seaborn as sns  
   sns.set\_theme()

   plot \= sns.histplot(df, x="column\_name")  
   plt.savefig("plot.png")  
   \#+end\_src

   Executing this block will generate plot.png and display it directly in the Org buffer.67

This literate programming approach, combined with the powerful version control capabilities of Magit (covered in Chapter 7), provides a more robust, transparent, and version-controllable environment for data science than traditional notebook formats.

## **Part V: The Integrated Professional's Hub**

This new section is dedicated to your specific goals of integrating modern AI assistants and building a powerful, automated email workflow directly within Emacs.

### **Chapter 12: The AI-Powered Assistant: Integrating LLMs**

Emacs's unparalleled extensibility makes it a perfect environment for integrating Large Language Models (LLMs) and AI coding assistants. You can build a workflow that leverages multiple services, choosing the right tool for the right task.

#### **GitHub Copilot**

GitHub Copilot provides powerful in-buffer code completion. The most popular Emacs integration is copilot.el. As it is not available on the main package archives, it's best installed directly from its source repository using a package manager like straight.el.78

Code snippet

;; Assumes you are using straight.el as your package manager  
(use-package copilot  
  :straight (copilot :type git :host github :repo "zerolfx/copilot.el")  
  :ensure t  
  :config  
  (add-hook 'prog-mode-hook 'copilot-mode))

After installation, run M-x copilot-login to authenticate. Copilot will then provide completion suggestions as you type in programming modes.78

#### **Claude Code**

Claude Code is a powerful AI assistant that operates on your entire project. Emacs integrations connect to the Claude Code command-line interface (CLI), bringing its capabilities into your editor. The claude-code-ide.el package is a strong choice, offering deep integration through the Model Context Protocol (MCP), which allows Claude to be aware of your open files, selected text, and even diagnostic information from your language server.80

Code snippet

;; Using straight.el  
(use-package claude-code-ide  
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")  
  :ensure t  
  :bind ("C-c C-'". claude-code-ide-menu))

This setup gives you a project-aware AI assistant that can perform multi-file refactoring and interact with your Emacs environment.80

#### **Gemini CLI**

Google's Gemini CLI is a versatile and powerful open-source AI agent that excels at coding tasks and can interact with your local file system and shell.82 While direct, feature-rich Emacs packages for the Gemini CLI are still emerging, you can integrate it effectively in two ways:

1. **Via a General-Purpose Tool:** Packages like aider.el provide a git-aware, chat-based interface for coding and can be configured to use Gemini as their backend model.83 This is an excellent way to get a structured, AI-powered workflow.  
2. **Via an Emacs Terminal:** You can run the Gemini CLI directly inside one of Emacs's superior terminal emulators, like vterm or eat. This allows you to interact with the CLI as intended, using its full feature set, including file system access and command execution, without leaving your editor.85

There are also lower-level Elisp libraries like google-gemini.el for interacting with the Gemini API directly, which are useful for building custom tools.86

### **Chapter 13: The Command Center: Email and Automation**

One of Emacs's greatest strengths is its ability to treat email as plain text, making it scriptable. This chapter will guide you through setting up a modern email client for your Outlook.com account and demonstrate how to begin automating your inbox.

#### **The Modern Emacs Email Stack: mu4e**

The recommended setup for handling large volumes of email efficiently is mu4e. It's not a single program but a combination of tools that work together 87:

* **mbsync** (from the isync package): A command-line tool to download your emails from the Outlook.com server via IMAP and store them locally in the Maildir format.  
* **mu**: A tool that creates a searchable index of all your local email files.  
* **mu4e**: The Emacs user interface that interacts with the mu index to display, search, and manage your email.  
* **msmtp**: A command-line tool for sending email via SMTP.

#### **Configuring for Outlook.com with OAuth2**

Connecting to a modern email service like Outlook.com requires specific settings and handling of modern authentication (OAuth2).89

1. **Server Settings:** You will need the following server details for your configuration files 91:  
   * **IMAP Server (for mbsync):** outlook.office365.com on port 993 with SSL/TLS.  
   * **SMTP Server (for msmtp):** smtp-mail.outlook.com on port 587 with STARTTLS.  
2. **Handling OAuth2 Authentication:** Standard username/password authentication is being phased out. You will need a helper program to manage the OAuth2 token exchange with Microsoft's servers. A common and effective tool for this is mutt\_oauth2.py.92 You configure  
   mbsync and msmtp to call this script to get a fresh access token whenever they need to connect, using the PassCmd option in their respective configuration files.92

#### **Automating Your Inbox with Elisp**

The true power of this setup is that all your emails are now local text files, fully accessible to Emacs Lisp. This unlocks limitless automation potential. You can write functions that trigger on certain events (like receiving new mail) or custom actions you can invoke on a message.

Here is a practical example of a custom action that directly addresses your goal. This code adds a new command to mu4e that, when triggered on an email, will create a new TODO item in your Org Agenda file with the email's sender and subject.

Code snippet

;; Add this to your init.el after configuring mu4e  
(add-to-list 'mu4e-view-actions  
  '("Create Org task". (lambda (msg)  
                          (let\* ((subject (mu4e-message-field msg :subject))  
                                 ;; mu4e-message-field returns a list of lists for addresses  
                                 (sender-list (mu4e-message-field msg :from))  
                                 (sender-name (caar sender-list)))  
                            ;; This requires you to have org-capture configured  
                            (org-capture-string  
                             (format "TODO Email from %s: %s" sender-name subject)  
                             "t")))) t)

With this function in your configuration, you can simply press a in the mu4e message view, select "Create Org task," and instantly turn an email into an actionable item in your productivity system. This is just the beginning; you can extend this to parse the email body for specific information, automatically file messages, or trigger any other scripted workflow you can imagine.93

## **Conclusion: The Lifelong Journey of an Emacs Artisan**

This handbook has charted a course from a clean slate to a highly sophisticated, specialized, and personalized Emacs environment. The journey began by establishing a solid foundation through a meticulous, platform-aware installation process that preempts the common frustrations that deter many newcomers. It then progressed to the core of the artisan's craft: adopting a declarative and reproducible package management strategy with use-package and straight.el, which ensures a stable and maintainable configuration over the long term. Upon this foundation, a modern core of essential packages—including the Vertico completion stack and the unparalleled Git interface, Magit—was assembled to create a fluid and efficient user experience. Finally, this powerful general-purpose environment was specialized for three distinct professional domains: software development, academic research, and data science, demonstrating its profound versatility.

The central theme of this guide is that an Emacs configuration is not a static artifact to be downloaded and used; it is a living project that is built, refined, and curated over time. It is a reflection of the user's evolving skills, workflows, and understanding of their own craft. The process of building this configuration is itself an act of learning. The path of the Emacs artisan is one of continuous, incremental refinement. The environment is never truly "finished." There is always a new package to discover, a workflow to optimize, or a custom function to write that shaves a few seconds off a repetitive task. This is the ultimate power and appeal of Emacs: it is not just a tool to be used, but a medium to be shaped.

To continue this journey, the following resources are invaluable:

* **The Official GNU Emacs Manual:** The definitive source of truth. Accessible within Emacs via C-h r.  
* **Mastering Emacs** by Mickey Petersen: An excellent book and website that provides deep dives into core Emacs concepts.  
* **System Crafters:** A community and collection of resources, including articles and videos, dedicated to mastering Emacs and other powerful tools.  
* **Community Configurations:** Studying the publicly available configurations of experienced Emacs users is one of the best ways to discover new packages and techniques.

Embrace the journey. The initial investment is significant, but the reward is a computing environment that is a true extension of its user's mind—an instrument of unparalleled power and precision.

#### **Works cited**

1. A Gentle Introduction to Emacs Configuration \- The Chronicle \- Aaron Bieber., accessed September 28, 2025, [https://blog.aaronbieber.com/2015/07/05/a-gentle-introduction-to-emacs-configuration.html](https://blog.aaronbieber.com/2015/07/05/a-gentle-introduction-to-emacs-configuration.html)  
2. Configuring emacs the right way \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/s33n7e/configuring\_emacs\_the\_right\_way/](https://www.reddit.com/r/emacs/comments/s33n7e/configuring_emacs_the_right_way/)  
3. GNU Emacs \- Guided Tour, accessed September 28, 2025, [https://www.gnu.org/software/emacs/tour/](https://www.gnu.org/software/emacs/tour/)  
4. Emacs for writers \- Fedora Magazine, accessed September 28, 2025, [https://fedoramagazine.org/emacs-for-writers/](https://fedoramagazine.org/emacs-for-writers/)  
5. Emacs: The Best Python Editor?, accessed September 28, 2025, [https://realpython.com/emacs-the-best-python-editor/](https://realpython.com/emacs-the-best-python-editor/)  
6. a guide to configure emacs \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/g2m2rg/a\_guide\_to\_configure\_emacs/](https://www.reddit.com/r/emacs/comments/g2m2rg/a_guide_to_configure_emacs/)  
7. patrickt/emacs: the greatest emacs setup of all time \- GitHub, accessed September 28, 2025, [https://github.com/patrickt/emacs](https://github.com/patrickt/emacs)  
8. Installing Emacs 29.1 from source on Debian 12 \- GitHub Gist, accessed September 28, 2025, [https://gist.github.com/zoliky/0445b20676bfa85450d7df006066ceb7](https://gist.github.com/zoliky/0445b20676bfa85450d7df006066ceb7)  
9. Emacs-30.2 \- Linux From Scratch\!, accessed September 28, 2025, [https://www.linuxfromscratch.org/blfs/view/svn/postlfs/emacs.html](https://www.linuxfromscratch.org/blfs/view/svn/postlfs/emacs.html)  
10. Emacs configuration \- Patrick D. Elliott, accessed September 28, 2025, [https://www.patrickdelliott.com/emacs.d/](https://www.patrickdelliott.com/emacs.d/)  
11. My Emacs Init \- Thomas Ingram, accessed September 28, 2025, [https://taingram.org/init.html](https://taingram.org/init.html)  
12. Emacs Config \- Jamie Collinson, accessed September 28, 2025, [https://jamiecollinson.com/blog/my-emacs-config/](https://jamiecollinson.com/blog/my-emacs-config/)  
13. Emacs from scratch \- Graham Marlow, accessed September 28, 2025, [https://www.mgmarlow.com/words/2022-05-02-learning-emacs/](https://www.mgmarlow.com/words/2022-05-02-learning-emacs/)  
14. What Emacs Configurations/Plugins would you consider essential? \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/hzpupp/what\_emacs\_configurationsplugins\_would\_you/](https://www.reddit.com/r/emacs/comments/hzpupp/what_emacs_configurationsplugins_would_you/)  
15. emacs writing to my init.el \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1jm1vr6/emacs\_writing\_to\_my\_initel/](https://www.reddit.com/r/emacs/comments/1jm1vr6/emacs_writing_to_my_initel/)  
16. Package Management in Emacs: The Good, the Bad and the Ugly \- Bozhidar Batsov, accessed September 28, 2025, [https://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/](https://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/)  
17. A beginner's Emacs config. In the first entry to this series, I'll… | by Zac Wood | Medium, accessed September 28, 2025, [https://medium.com/@zac.wood9/a-beginners-emacs-config-44400bcf54a1](https://medium.com/@zac.wood9/a-beginners-emacs-config-44400bcf54a1)  
18. Configure Emacs Writing Studio for authors \- Lucid Manager, accessed September 28, 2025, [https://lucidmanager.org/productivity/configure-emacs/](https://lucidmanager.org/productivity/configure-emacs/)  
19. jwiegley/use-package: A use-package declaration for simplifying your .emacs \- GitHub, accessed September 28, 2025, [https://github.com/jwiegley/use-package](https://github.com/jwiegley/use-package)  
20. What are the best practices in laying out an init.el that won't turn into spaghetti after a year or two? : r/emacs \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1ata6ag/what\_are\_the\_best\_practices\_in\_laying\_out\_an/](https://www.reddit.com/r/emacs/comments/1ata6ag/what_are_the_best_practices_in_laying_out_an/)  
21. A Quick Tutorial on Use-package for Emacs | Ian Y.E. Pan, accessed September 28, 2025, [https://ianyepan.github.io/posts/setting-up-use-package/](https://ianyepan.github.io/posts/setting-up-use-package/)  
22. Getting Started with Use-Package, accessed September 28, 2025, [https://cachestocaches.com/2015/8/getting-started-use-package/](https://cachestocaches.com/2015/8/getting-started-use-package/)  
23. use-package User Manual \- GNU, accessed September 28, 2025, [https://www.gnu.org/software/emacs/manual/html\_node/use-package/](https://www.gnu.org/software/emacs/manual/html_node/use-package/)  
24. Using use-package the right way \- (think) \- Bozhidar Batsov, accessed September 28, 2025, [https://batsov.com/articles/2025/04/17/using-use-package-the-right-way/](https://batsov.com/articles/2025/04/17/using-use-package-the-right-way/)  
25. use-package User Manual \- GNU, accessed September 28, 2025, [https://www.gnu.org/software/emacs/manual/html\_mono/use-package.html](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html)  
26. Advanced Emacs Package Management with straight.el \- System Crafters, accessed September 28, 2025, [https://systemcrafters.net/advanced-package-management/using-straight-el/](https://systemcrafters.net/advanced-package-management/using-straight-el/)  
27. Strategies to manage Emacs packages \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/wujmvx/strategies\_to\_manage\_emacs\_packages/](https://www.reddit.com/r/emacs/comments/wujmvx/strategies_to_manage_emacs_packages/)  
28. Emacs Configuration | Phundrak's Dotfiles, accessed September 28, 2025, [https://config.phundrak.com/emacs/](https://config.phundrak.com/emacs/)  
29. From Doom to Vanilla Emacs \- Blog \- dornea.nu, accessed September 28, 2025, [https://blog.dornea.nu/2024/02/22/from-doom-to-vanilla-emacs/](https://blog.dornea.nu/2024/02/22/from-doom-to-vanilla-emacs/)  
30. Literate Emacs init.el (Emacs Literate Config) | Ivory Siege Tower, accessed September 28, 2025, [https://siegetower.pages.dev/emacs-literate-config/literate-emacs-initel](https://siegetower.pages.dev/emacs-literate-config/literate-emacs-initel)  
31. Quick start \- Spacemacs, accessed September 28, 2025, [https://www.spacemacs.org/doc/QUICK\_START.html](https://www.spacemacs.org/doc/QUICK_START.html)  
32. New to Emacs: What are layers : r/spacemacs \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/spacemacs/comments/5px6xg/new\_to\_emacs\_what\_are\_layers/](https://www.reddit.com/r/spacemacs/comments/5px6xg/new_to_emacs_what_are_layers/)  
33. Whats the difference between Doom emacs and spacemacs \- Hacker News, accessed September 28, 2025, [https://news.ycombinator.com/item?id=35624730](https://news.ycombinator.com/item?id=35624730)  
34. Compare Doom-emacs, Spacemacs, and vanilla Emacs \- Yiming Chen, accessed September 28, 2025, [https://yiming.dev/blog/2018/01/22/compare-doom-emacs-spacemacs-vanilla-emacs/](https://yiming.dev/blog/2018/01/22/compare-doom-emacs-spacemacs-vanilla-emacs/)  
35. doomemacs/doomemacs: An Emacs framework for the ... \- GitHub, accessed September 28, 2025, [https://github.com/doomemacs/doomemacs](https://github.com/doomemacs/doomemacs)  
36. Is it worth it to switch to Doom/Spacemacs from vanilla? : r/emacs \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1ak8zk5/is\_it\_worth\_it\_to\_switch\_to\_doomspacemacs\_from/](https://www.reddit.com/r/emacs/comments/1ak8zk5/is_it_worth_it_to_switch_to_doomspacemacs_from/)  
37. Moving from Spacemacs to Doom Emacs \- Shrutarshi Basu, accessed September 28, 2025, [https://v4.basus.me/journal/2024/07/moving-from-spacemacs-to-doom-emacs/](https://v4.basus.me/journal/2024/07/moving-from-spacemacs-to-doom-emacs/)  
38. Completion system \- \- Decentralized meta-learning, accessed September 28, 2025, [https://blog.costan.ro/post/2022-02-22-emacs-completion-system/](https://blog.costan.ro/post/2022-02-22-emacs-completion-system/)  
39. Can someone explain thr completion frameworks? : r/emacs \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1dahqw7/can\_someone\_explain\_thr\_completion\_frameworks/](https://www.reddit.com/r/emacs/comments/1dahqw7/can_someone_explain_thr_completion_frameworks/)  
40. Essential Emacs Packages for Efficient Software Development and ..., accessed September 28, 2025, [https://www.jamescherti.com/essential-emacs-packages/](https://www.jamescherti.com/essential-emacs-packages/)  
41. Emacs: completion framework (Embark,Consult,Orderless,etc.) \- YouTube, accessed September 28, 2025, [https://www.youtube.com/watch?v=43Dg5zYPHTU](https://www.youtube.com/watch?v=43Dg5zYPHTU)  
42. It's Magit\! A Git Porcelain inside Emacs, accessed September 28, 2025, [https://magit.vc/](https://magit.vc/)  
43. Mastering Git with Magit \- Getting Started \- System Crafters, accessed September 28, 2025, [https://systemcrafters.net/mastering-git-with-magit/introduction/](https://systemcrafters.net/mastering-git-with-magit/introduction/)  
44. Emacs Magit User Manual, accessed September 28, 2025, [https://magit.vc/manual/magit.html](https://magit.vc/manual/magit.html)  
45. An introduction to Magit, an Emacs mode for Git : r/programming \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/programming/comments/1jhsyic/an\_introduction\_to\_magit\_an\_emacs\_mode\_for\_git/](https://www.reddit.com/r/programming/comments/1jhsyic/an_introduction_to_magit_an_emacs_mode_for_git/)  
46. Better Emacs Config: use-package · menno.io, accessed September 28, 2025, [https://menno.io/posts/use-package/](https://menno.io/posts/use-package/)  
47. Getting started with Magit \- Kisaragi Hiu, accessed September 28, 2025, [https://kisaragi-hiu.com/blog/2018-09-20-magit-introduction/](https://kisaragi-hiu.com/blog/2018-09-20-magit-introduction/)  
48. Magit (Emacs) as git client :: ArMD — Architect Manager Devloper, accessed September 28, 2025, [https://thearjunmdas.github.io/entries/magit-emacs-as-git-client/](https://thearjunmdas.github.io/entries/magit-emacs-as-git-client/)  
49. What are some must-have packages for emacs? \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/w4gxoa/what\_are\_some\_musthave\_packages\_for\_emacs/](https://www.reddit.com/r/emacs/comments/w4gxoa/what_are_some_musthave_packages_for_emacs/)  
50. How is Emacs used in a professional setting? \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1e1dksm/how\_is\_emacs\_used\_in\_a\_professional\_setting/](https://www.reddit.com/r/emacs/comments/1e1dksm/how_is_emacs_used_in_a_professional_setting/)  
51. Configuring Emacs as a C/C++ IDE \- LSP Mode, accessed September 28, 2025, [https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/](https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/)  
52. Building Your First Emacs Config | Sophie Bosio, accessed September 28, 2025, [https://sophiebos.io/posts/first-emacs-config/](https://sophiebos.io/posts/first-emacs-config/)  
53. Python Development on Emacs \- Shiv Deepak, accessed September 28, 2025, [https://shivdeepak.com/posts/linting-in-python/](https://shivdeepak.com/posts/linting-in-python/)  
54. How do you create a robust Python IDE with Emacs (as the Text editor), accessed September 28, 2025, [https://emacs.stackexchange.com/questions/9696/how-do-you-create-a-robust-python-ide-with-emacs-as-the-text-editor](https://emacs.stackexchange.com/questions/9696/how-do-you-create-a-robust-python-ide-with-emacs-as-the-text-editor)  
55. thinkhuman/writingwithemacs: Tips, Examples, and Resources for Writing with Emacs, accessed September 28, 2025, [https://github.com/thinkhuman/writingwithemacs](https://github.com/thinkhuman/writingwithemacs)  
56. Emacs Org Mode for Distraction-Free Writing \- Lucid Manager, accessed September 28, 2025, [https://lucidmanager.org/productivity/emacs-for-distraction-free-writing/](https://lucidmanager.org/productivity/emacs-for-distraction-free-writing/)  
57. Emacs for Data Science, accessed September 28, 2025, [https://ahsanijaz.github.io/2016-09-20-Emacs/](https://ahsanijaz.github.io/2016-09-20-Emacs/)  
58. Org mode, Denote, Howm etc, which do you use and why? : r/emacs \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1mox889/org\_mode\_denote\_howm\_etc\_which\_do\_you\_use\_and\_why/](https://www.reddit.com/r/emacs/comments/1mox889/org_mode_denote_howm_etc_which_do_you_use_and_why/)  
59. My org-roam workflows for taking notes and writing articles \- Dominik Honnef, accessed September 28, 2025, [https://honnef.co/articles/my-org-roam-workflows-for-taking-notes-and-writing-articles/](https://honnef.co/articles/my-org-roam-workflows-for-taking-notes-and-writing-articles/)  
60. Loving Emacs Writing Studio \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1j4370o/loving\_emacs\_writing\_studio/](https://www.reddit.com/r/emacs/comments/1j4370o/loving_emacs_writing_studio/)  
61. Bibliographic Notes with the Citar-Denote Emacs Package \- Lucid Manager, accessed September 28, 2025, [https://lucidmanager.org/productivity/bibliographic-notes-in-emacs-with-citar-denote/](https://lucidmanager.org/productivity/bibliographic-notes-in-emacs-with-citar-denote/)  
62. Create and manage literature notes with Citar-Denote \- Emacs Writing Studio \- YouTube, accessed September 28, 2025, [https://www.youtube.com/watch?v=s7Mf6udiCSE](https://www.youtube.com/watch?v=s7Mf6udiCSE)  
63. pprevos/citar-denote: Emacs package to create and retrieve bibliography notes with the Citar and Denote packages. \- GitHub, accessed September 28, 2025, [https://github.com/pprevos/citar-denote](https://github.com/pprevos/citar-denote)  
64. List all roam notes citing a citar note \- How To \- Org-roam \- Discourse, accessed September 28, 2025, [https://org-roam.discourse.group/t/list-all-roam-notes-citing-a-citar-note/3135](https://org-roam.discourse.group/t/list-all-roam-notes-citing-a-citar-note/3135)  
65. Emacs for Data Science \- Robert Vesco, accessed September 28, 2025, [https://www.robertvesco.com/posts/emacs-for-data-science/](https://www.robertvesco.com/posts/emacs-for-data-science/)  
66. Emacs for Data Science | by Insight \- Medium, accessed September 28, 2025, [https://medium.com/insight-data/emacs-for-data-science-af814b78eb41](https://medium.com/insight-data/emacs-for-data-science-af814b78eb41)  
67. Visualising data analysis in Emacs org-mode \- akuszyk.com, accessed September 28, 2025, [https://akuszyk.com/2023-11-18-using-emacs-org-mode-as-a-jupyter-notebook.html](https://akuszyk.com/2023-11-18-using-emacs-org-mode-as-a-jupyter-notebook.html)  
68. How to Install GNU Emacs 30.2 in Ubuntu 24.04, 22.04 via PPA \- UbuntuHandbook, accessed September 28, 2025, [https://ubuntuhandbook.org/index.php/2023/08/gnu-emacs-29-1-ubuntu-ppa/](https://ubuntuhandbook.org/index.php/2023/08/gnu-emacs-29-1-ubuntu-ppa/)  
69. How to Install the Latest Emacs on Ubuntu \- It's FOSS, accessed September 28, 2025, [https://itsfoss.com/install-emacs-ubuntu/](https://itsfoss.com/install-emacs-ubuntu/)  
70. Emacs 30 on Ubuntu 24.04 \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/1kmnq5o/emacs\_30\_on\_ubuntu\_2404/](https://www.reddit.com/r/emacs/comments/1kmnq5o/emacs_30_on_ubuntu_2404/)  
71. What libraries do I need to install if I want to compile Emacs? \- Ask Ubuntu, accessed September 28, 2025, [https://askubuntu.com/questions/213873/what-libraries-do-i-need-to-install-if-i-want-to-compile-emacs](https://askubuntu.com/questions/213873/what-libraries-do-i-need-to-install-if-i-want-to-compile-emacs)  
72. Installing Emacs on Ubuntu: build from source or use PPA \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/94ki9n/installing\_emacs\_on\_ubuntu\_build\_from\_source\_or/](https://www.reddit.com/r/emacs/comments/94ki9n/installing_emacs_on_ubuntu_build_from_source_or/)  
73. My lil' Ghostty terminal config \- Birchtree, accessed September 28, 2025, [https://birchtree.me/blog/my-lil-ghosty-terminal-config-2/](https://birchtree.me/blog/my-lil-ghosty-terminal-config-2/)  
74. exec-path-from-shell message when starting emacs \- Stack Overflow, accessed September 28, 2025, [https://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs](https://stackoverflow.com/questions/35286203/exec-path-from-shell-message-when-starting-emacs)  
75. exec-path-from-shell \- NonGNU ELPA, accessed September 28, 2025, [https://elpa.nongnu.org/nongnu/exec-path-from-shell.html](https://elpa.nongnu.org/nongnu/exec-path-from-shell.html)  
76. exec-path and $PATH \- Emacs Stack Exchange, accessed September 28, 2025, [https://emacs.stackexchange.com/questions/550/exec-path-and-path](https://emacs.stackexchange.com/questions/550/exec-path-and-path)  
77. Using configuration management tools (Salt, Ansible, Chef, Puppet) to manage Emacs configuration and packages \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/7aoodx/using\_configuration\_management\_tools\_salt\_ansible/](https://www.reddit.com/r/emacs/comments/7aoodx/using_configuration_management_tools_salt_ansible/)  
78. Setting up Github Copilot in Emacs | Robert Krahn, accessed September 28, 2025, [https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/](https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/)  
79. copilot-emacs/copilot.el: An unofficial Copilot plugin for ... \- GitHub, accessed September 28, 2025, [https://github.com/copilot-emacs/copilot.el](https://github.com/copilot-emacs/copilot.el)  
80. Claude Code IDE integration for Emacs \- GitHub, accessed September 28, 2025, [https://github.com/manzaltu/claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el)  
81. Claude Code in Emacs | Will Schenk, accessed September 28, 2025, [https://willschenk.com/howto/2025/claude\_code\_in\_emacs/](https://willschenk.com/howto/2025/claude_code_in_emacs/)  
82. Gemini CLI | Gemini for Google Cloud, accessed September 28, 2025, [https://cloud.google.com/gemini/docs/codeassist/gemini-cli](https://cloud.google.com/gemini/docs/codeassist/gemini-cli)  
83. tninja/aider.el: AI assisted programming in Emacs with Aider \- GitHub, accessed September 28, 2025, [https://github.com/tninja/aider.el](https://github.com/tninja/aider.el)  
84. Integrating Generative AI coding assistance in Emacs, accessed September 28, 2025, [https://emacs.stackexchange.com/questions/82170/integrating-generative-ai-coding-assistance-in-emacs](https://emacs.stackexchange.com/questions/82170/integrating-generative-ai-coding-assistance-in-emacs)  
85. Hands-on with Gemini CLI \- Google Codelabs, accessed September 28, 2025, [https://codelabs.developers.google.com/gemini-cli-hands-on](https://codelabs.developers.google.com/gemini-cli-hands-on)  
86. emacs-openai/google-gemini: Elisp library for the Google Gemini API \- GitHub, accessed September 28, 2025, [https://github.com/emacs-openai/google-gemini](https://github.com/emacs-openai/google-gemini)  
87. Emacs as an Email Client \- Yann Herklotz, accessed September 28, 2025, [https://yannherklotz.com/emacs-email-client/](https://yannherklotz.com/emacs-email-client/)  
88. Email in emacs, I want to, but wow, it's overwhelming \- Reddit, accessed September 28, 2025, [https://www.reddit.com/r/emacs/comments/4rl0a9/email\_in\_emacs\_i\_want\_to\_but\_wow\_its\_overwhelming/](https://www.reddit.com/r/emacs/comments/4rl0a9/email_in_emacs_i_want_to_but_wow_its_overwhelming/)  
89. How to use emacs to read and send email from outlook.office.com?, accessed September 28, 2025, [https://emacs.stackexchange.com/questions/82959/how-to-use-emacs-to-read-and-send-email-from-outlook-office-com](https://emacs.stackexchange.com/questions/82959/how-to-use-emacs-to-read-and-send-email-from-outlook-office-com)  
90. Does Emacs/Gnus on Linux support Modern Authentication for accessing Outlook.com?, accessed September 28, 2025, [https://learn.microsoft.com/en-us/answers/questions/4664650/does-emacs-gnus-on-linux-support-modern-authentica](https://learn.microsoft.com/en-us/answers/questions/4664650/does-emacs-gnus-on-linux-support-modern-authentica)  
91. POP, IMAP, and SMTP settings for Outlook.com \- Microsoft Support, accessed September 28, 2025, [https://support.microsoft.com/en-au/office/pop-imap-and-smtp-settings-for-outlook-com-d088b986-291d-42b8-9564-9c414e2aa040](https://support.microsoft.com/en-au/office/pop-imap-and-smtp-settings-for-outlook-com-d088b986-291d-42b8-9564-9c414e2aa040)  
92. Local email from Office365 using OAUTH2 with mbsync | Simon Dobson, accessed September 28, 2025, [https://simondobson.org/2024/02/03/getting-email/](https://simondobson.org/2024/02/03/getting-email/)  
93. Elisp: Simple Code Examples, accessed September 28, 2025, [http://xahlee.info/emacs/emacs/elisp\_examples.html](http://xahlee.info/emacs/emacs/elisp_examples.html)  
94. A Complete Guide to Email in Emacs using Mu and Mu4e, accessed September 28, 2025, [https://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and/](https://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and/)  
95. Compose and Send Email with Emacs \- System Crafters, accessed September 28, 2025, [https://systemcrafters.net/emacs-mail/compose-and-send-email/](https://systemcrafters.net/emacs-mail/compose-and-send-email/)