---
layout:     post
title:      "A lean and minimal Emacs configuration"
date:       2023-01-25 08:23:25 PST
categories: emacs editors
---

<small>*Note: I may or may not be the original author of various scripts used in this
post. Since they are acquired/written over several years, I honestly don't
remember the history/source of each. If its not me, its probably from EmacsWiki
or Reddit.*</small>

Emacs is an amazing text editor for almost anything. However, it takes time and
experimentation to make it feel more productive than traditional options out
there (Visual Studio Code, Sublime, IntelliJ, Google Docs ...). Often, we end up
with giant configurations with dozens of scripts and external packages. They are
great, practical and productive, but also slow to install and start.

This got me thinking if I can trim down my configuration into a single `.emacs`
file, that only depends on builtin/shipped packages. Turns out, you can! And you
don't really lose much. Particularly, if you are using Emacs 29. In this post, I
will go through a lean, minimal and productive single file `.emacs`
configuration.

The goals of this configuration are:

- Be lightweight, both in size and load times.
- Should work reasonably in terminal mode.
- Sane defaults and keybindings.
- Quick searching and moving around.
- Some modern features like auto-completion, project search, version control.
- Tailed for software development.
- Only use the packages shipped in vanilla Emacs.

#### Where is it?

You can find the complete configuration [in this GitHub
Gist](https://gist.github.com/vishesh/8aa9dec6b0097b9c5e14688638831c6a).

## Let's Start

First, we will configure the package manager and
[`use-package`](https://github.com/jwiegley/use-package) (ships with Emacs 29,
but also available in ELPA). Highly recommended if you are not using it already.
It's a very [standard
configuration](https://gist.github.com/vishesh/8aa9dec6b0097b9c5e14688638831c6a#file-emacs-L21-L51).

## Basic text editing

For general editing, I like to set these defaults:

```elisp
(prefer-coding-system 'utf-8)

;; Default major mode for new files.
(setq-default major-mode 'text-mode)

;; History of copied text.
(setq kill-ring-max 500)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)

;; Don't wrap lines.
(setq-default truncate-lines t)
```

**Delete Selection mode** does copy/paste in a bit more traditional way. It replaces
the selected text when you paste something.

```elisp
(use-package delsel
  :hook (after-init . delete-selection-mode))

```

Next we change the behavior of some basic keybindings for moving around. The
variable names imply what they do.

```elisp
(bind-key "C-a"     'back-to-indentation-or-beginning-of-line)
(bind-key "M-g"     'goto-line)
(bind-key "C-M-'"   'match-paren)
(bind-key "C-w"     'kill-region-or-backward-word)
(bind-key "M-w"     'kill-region-or-thing-at-point)
(bind-key "C-S-k"   'kill-whole-line)
(bind-key "C-o"     'open-line-below)
(bind-key "C-c C-o" 'open-line-above)
(bind-key "C-S-o"   'open-line-above)
(bind-key "M-j"     'join-line)
```

In case, you are not familiar with Emacs terminology, (1) kill = copy, (2) yank
= paste, (3) region = selection. These tweaks are mainly tailed for programming,
as they take indentation and symbols into consideration unlike the defaults.

```elisp
(defun back-to-indentation-or-beginning-of-line ()
  "Moves cursor to beginning of line, taking indentation into account"
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
    
(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
  Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))
   
(defun kill-region-or-thing-at-point (beg end)
  "If a region is active kill it, or kill the thing (word/symbol) at point"
  (interactive "r")
  (unless (region-active-p)
    (save-excursion
      (setq beg (re-search-backward "\\_<" nil t))
      (setq end (re-search-forward "\\_>" nil t))))
  (kill-ring-save beg end))

(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun open-line-below ()
  "Starts a new line below the current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above ()
  "Starts a new line above the current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
```

Next, we add **rectangle-mode** which I primarily use for adding or deleting
text from multiple lines. Alternatively,
[iedit-mode](https://github.com/victorhge/iedit) and
[multiple-cursors](https://github.com/magnars/multiple-cursors.el) are excellent
unofficial choices.

```elisp
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))
```

**Goto Address mode** identifies URLs and makes them clickable.

```elisp
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))
```

We'll get into more sophisticated completion, but **hippie-expand** is quick and
convenient, and I don't mind having a dedicated keybinding for it.

```elisp
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name try-expand-all-abbrevs
        try-expand-list try-expand-line try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(bind-key "M-/" 'hippie-expand)
```

## Searching

I like to quickly search the currently open file. `M-s o` is already a good
default for invoking **occur**. For incremental search, I just add couple tweaks
that tries to add some familiar keybindings to result selection:

```elisp
;; Make backspace delete the search term not goto last address
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defun isearchp-kill-ring-save ()
  "Copy the current search string to the kill ring. Credits: isearch+"
  (interactive)
  (kill-new isearch-string)
  (sit-for 1)
  (isearch-update))

;; Default query-replace shortcut on macOS would conflict with system shortcut
;; to take screenshot. So lets rebind these.
(bind-key "C-?" #'query-replace)
(bind-keys :map isearch-mode-map
           ("C-?" . isearch-query-replace)
           ("M-w" . isearchp-kill-ring-save))
```

Good integration with **grep** or **ripgrep** is critical to my work. Emacs,
already ships with `grep` package that has many useful commands (`grep`/`rgrep`
is what I mostly use). However, they always opens up a new result buffer, which
quickly gets annoying for short lived greps. Packages like
[consult](https://github.com/minad/consult) and
[counsel](https://github.com/abo-abo/swiper) address this issue very well.

For this config, we'll just hack up a quick fix. (1) It takes the selected
region or symbol under point, (2) invokes grep/ripgrep, (3) displays results as
minibuffer completions. It is based on `completing-read`, the elisp function
used by the [minibuffer
completion](https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html)
system (by default bound to `TAB`).

```elisp
(bind-key "C-c g" 'grep-completing-read)

(defun grep-completing-read ()
  "Execute grep/ripgrep search using completing-read"
  (interactive)
  (let* ((default-term (if (region-active-p)
                           (substring-no-properties
                            (buffer-substring (mark) (point)))
                         (thing-at-point 'symbol)))
         (term (read-string "search for: " default-term))
         (execute-search
          (lambda (term)
            (if (executable-find "rg")
                (process-lines "rg" "--line-number" term)
              (process-lines "git" "grep" "-niH" "-e" term))))
         (results (funcall execute-search term))
         (line-list (split-string (completing-read "results: " results) ":"))
         (rfile (car line-list))
         (rlnum (string-to-number (car (cdr line-list)))))
    (find-file rfile)
    (goto-line rlnum)
    (recenter)))
```

I recommend using this with **icomplete/fido** completion modes, which we'll
look at later in this post. **Project** and **Version Control (VC)** packages
come with few convenient `grep` commands of its own.

## Buffers and Windows

These are my preferred bindings for quickly switching between buffers.

```elisp
(bind-key "M-_"     'previous-buffer)
(bind-key "M-+"     'next-buffer)
(bind-key "C-x C-b" 'switch-to-buffer)
```

**IBuffer** is an excellent package, which I mostly use to kill several buffers
together. You write your own function to group buffers together. There are
unofficial packages to automatically create groups based on active project,
projectile or VCS repository.

```elisp
(use-package ibuffer :bind (("C-x b"   . ibuffer-bs-show)))
```

**Auto Revert** automatically updates my buffers that got changed by other
programs. **Save Place** saves current cursor position, and restores it when you
open the same file later. **Recentf** simply keeps track of recent files 
(`C-c <RET>` is quite standard binding for it).

```elisp
(use-package autorevert
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  ;; lazy load recentf
  :hook (find-file . (lambda () (unless recentf-mode
                                  (recentf-mode)
                                  (recentf-track-opened-file))))
  :bind ("C-c <RET>" . recentf)
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".objs")
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))
```

Emacs really suffers when you open **large files**. Nowhere near the perfect
solution, but it does make it tolerable:

```elisp
(defun fast-file-view-mode ()
  "Makes the buffer readonly and disables fontlock and other bells and whistles
   for faster viewing"
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (font-lock-mode -1)
  (when (boundp 'anzu-mode)
    (anzu-mode -1)))

(defun large-find-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (fast-file-view-mode)))

(add-hook 'find-file-hook 'large-find-file-hook)
```

## Moving around windows

Unlike most IDEs and many text editors, Emacs GUI is very minimal. This is
great, as there is even more screen real-estate to split windows. Standard
keybindings `C-x {2, 3, 1, 0}` work great for creating splits. I have these
additional keybindings to move around. Since I use them a lot more than creating
splits, I selected keybindings that are easy to repeat.

```elisp
;; Cycles through windows.
(bind-key "C-q" 'other-window)

;; Directional window-selection routines. Binds shift-{left,up,right,down}
;; to move around windows.
(use-package windmove
  :ensure nil
  :hook (after-init . windmove-default-keybindings))

```

**Winner** mode keeps track of changes in window configuration, and lets you
undo/redo changes using `C-c <Left>`/`C-c <Right>`.

```elisp
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))
```

## Highlighting

Some are just for appearance, and others are actually useful. But they all
ship with emacs and are fairly lightweight. So, why not!

- **hl-line** to highlight current line.
  ```elisp
  (use-package hl-line
    :hook (after-init . global-hl-line-mode))
  ```
- **paren** for highlighting matching parenthesis/brackets.
  ```elisp
  (use-package paren
    :hook (after-init . show-paren-mode)
    :config
    (setq show-paren-when-point-inside-paren t
          show-paren-when-point-in-periphery t))
  ```
- **whitespace** to highlight bad whitespaces. Particularly useful for source
  code.
  
  ```elisp
  (use-package whitespace
    :ensure nil
    :bind ("C-x w" . whitespace-mode)
    :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
    :config
    (setq whitespace-line-column fill-column) ;; limit line length

    ;; Automatically clean up bad whitespace
    (setq whitespace-action '(auto-cleanup))

    ;; Only show bad whitespace
    (setq whitespace-style '(face trailing indentation empty)))
  ```
  
In my regular config, I use
[symbol-overlay](https://github.com/wolray/symbol-overlay) to highlight and
navigate symbols. In vanilla emacs, we can instead use the `highlight` package
(check `highlight-symbol-at-point` and `highlight-regexp`).

```elisp
(use-package hi-lock
  :bind
  (("M-n"  .   isearch-forward-symbol-at-point)
   ("M-l"  .   my/toggle-mark-symbol-at-point))
  :config
  (defun my/toggle-mark-symbol-at-point ()
    (interactive)
    (if hi-lock-interactive-patterns
        (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
      (highlight-symbol-at-point))))
```

Lastly, **pulse** can flash a line after certain events (e.g. switch window, jump
to definition/declaration/error, etc ..).

```elisp
;; Source: Centaur Emacs
(use-package pulse
  :ensure nil
  :preface
  (defun my-pulse-momentary (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point) 'next-error))

  (defun my-recenter (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary))

  :hook ((bookmark-after-jump next-error other-window
                              dumb-jump-after-jump imenu-after-jump) . my-recenter)
  :init (dolist (cmd '(recenter-top-bottom
                       other-window windmove-do-window-select
                       pop-to-mark-command pop-global-mark
                       pager-page-down pager-page-up))
          (advice-add cmd :after #'my-pulse-momentary)))
```

## Minibuffer Completion

Completions are awesome, whether it is code auto-completion, or file/command
suggestions. Emacs comes with two completion systems, (1) default minibuffer
completion (2) icomplete/fido.

First lets set tab as the completion trigger. In minibuffer, tab will trigger
completion.

```elisp
(setq tab-always-indent 'complete)
```

Minibuffer completion is very bare bones, and shows up completions in a
dedicated buffer. It's not my preferred way, but is still worth having it
configured in a way that feels slightly more natural. It also sets few a
variables that are relevant to `icomplete`. The most notable one is partial and
flex matching.

```elisp
(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-mode-map
              ("C-n" . minibuffer-next-completion)
              ("C-p" . minibuffer-previous-completion)
              ("M-<RET>" . my/minibuffer-choose-completion))
  :bind (:map completion-in-region-mode-map
              ("C-n" . minibuffer-next-completion)
              ("C-p" . minibuffer-previous-completion)
              ("M-<RET>" . my/minibuffer-choose-completion))
  :init
  (setq enable-recursive-minibuffers t
        completion-styles '(initials partial-completion flex)
        completions-format 'one-column
        completions-header-format nil
        completion-auto-help t
        completion-show-help nil
        completions-max-height 10
        resize-mini-windows t
        completion-flex-nospace nil
        completion-pcm-complete-word-inserts-delimiters t
        completion-auto-select nil
        completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t)

  (defun my/minibuffer-choose-completion (&optional no-exit no-quit)
    (interactive "P")
    (with-minibuffer-completions-window
      (let ((completion-use-base-affixes nil))
        (choose-completion nil no-exit no-quit)))))
```

My preference is to use **icomplete**/**fido** mode vertically, which feels a
bit more like [vertico](https://github.com/minad/vertico) and
[ivy](https://github.com/abo-abo/swiper) packages.

```elisp
(use-package icomplete
  :bind (:map icomplete-fido-mode-map
              ("TAB" . icomplete-force-complete)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions))
  :hook (icomplete-minibuffer-setup . (lambda () (setq-local truncate-lines t)))
  :init
  (setq icomplete-in-buffer t
        icomplete-with-completion-tables t
        icomplete-hide-common-prefix nil
        icomplete-prospects-height 6
        icomplete-compute-delay 0)
  (fido-mode +1)
  (fido-vertical-mode +1))
```

To invoke auto-completion when editing buffers, I've a dedicated keybinding:

```elisp
(bind-key "M-s i" 'completion-at-point)
```

However, it will not show completion using `fido-mode` and fallback to the old
minibuffer completion system. To make that happen, we will have to implement
`completion-in-region-function` on top of `completing-read`. As mentioned
earlier, `completing-read` is the function behind minibuffer completion, and now
its changed by `icomplete`.

```elisp
;; For making completion-at-point work with icomplete/fido.
;;   https://www.reddit.com/r/emacs/comments/ulsrb5/what_have_you_recently_removed_from_your_emacs/
(defun completing-read-at-point (start end col &optional pred)
  (if (minibufferp) (completion--in-region start end col pred)
    (let* ((init (buffer-substring-no-properties start end))
           (all (completion-all-completions init col pred (length init)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (completing-read "Completions: " col pred t init)))))
      (if completion
          (progn
            (delete-region start end)
            (insert completion)
            t)
        (message "No completions") nil))))

(setq completion-in-region-function #'completing-read-at-point)
```

Its still not ideal, and if I'm spending non-trivial time on a machine with this
config, I just install
[corfu](https://www.google.com/search?client=safari&rls=en&q=corfu+emacs&ie=UTF-8&oe=UTF-8)
or [company](https://company-mode.github.io).

Lastly, on top of completion, you can save the minibuffer history using
**savehist**.

```elisp
(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 60))
```

## Programming

Let's start with the indentation settings. This will expand tabs to space, so
skip it if you prefer tabs.

```elisp
(setq-default c-basic-offset    4
              tab-width         4
              indent-tabs-mode nil)
```

Next, we add an indicator column that helps us keep the line width under
control.

```elisp
(setq-default fill-column       100)
(global-display-fill-column-indicator-mode)
```

Close brackets/quotes automatically using **electric-pair-mode**:

```elisp
(electric-pair-mode +1)
```

Enable line numbers using **display-line-numbers-mode**:

```elisp
(bind-key "C-6"     'global-display-line-numbers-mode)

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . which-function-mode)
         (markdown-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)))
```

Some extra keybindings to indent text left or right manually:

```elisp
(bind-keys ("C-M->" . indent-rigidly-right)
           ("C-M-<" . indent-rigidly-left))
```

More helpers bindings for common programming tasks:

```elisp
(bind-key "C-c c"  'compile)
(bind-key "M-;"    'comment-dwim)
(bind-key "C-7"    'comment-line)
```

**IMenu** displays the outline of the current buffer. Really great way to find
classes/functions in the current buffer and jump to them.

```elisp
(bind-key "M-\\"    'imenu)
```

Next, we'll set up **eglot**, which is the official Language Server Protocol
(LSP) client since Emacs 29. This package, in my opinion, makes this
*no-external-dependency* requirement actually practical for long term use.

```elisp
(use-package eglot
  :hook ((c++-mode        . eglot-ensure)
         (c-mode          . eglot-ensure)
         (python-mode     . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c a r"           . #'eglot-rename)
              ("C-c h"             . #'eldoc)
              ("C-<down-mouse-1>"  . #'xref-find-definitions)
              ("C-S-<down-mouse-1>". #'xref-find-references)
              ("C-c C-c"           . #'eglot-code-actions)
              ("C-M-<tab>"         . #'eglot-format))
  :config
  (defun eglot-clangd-find-other-file (&optional new-window)
    "Switch between the corresponding C/C++ source and header file."
    (interactive)
    (let* ((server (eglot--current-server-or-lose))
           (rep
            (jsonrpc-request
             server
             :textDocument/switchSourceHeader
             (eglot--TextDocumentIdentifier))))
      (unless (equal rep nil)
        (funcall #'find-file (eglot--uri-to-path rep)))))

  ;; I mostly use C/C++ for work. My favorite option here is 
  ;; `--all-scopes-completion`, which looks into modules that are not 
  ;; yet included. Makes coding feel way more fluid and interruption free.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(c-mode c++-mode
                          . ("clangd"
                             "-j=4"
                             "--background-index"
                             "--clang-tidy"
                             "--all-scopes-completion"
                             "--completion-style=detailed"
                             "--header-insertion=iwyu"
                             "--header-insertion-decorators=0"
                             "--malloc-trim"
                             "--cross-file-rename"
                             "--pch-storage=memory"))))
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t
        eldoc-idle-delay 0.1
        eglot-autoshutdown t))
        
;; Use the `eglot-clangd-find-other-file` implemented above, to quickly switch
;; b/w source and header files. If you are not using LSP, there is
;; `ff-find-other-file`.
(use-package cc-mode
  :bind (:map c-mode-base-map
              ("C-c o" . eglot-clangd-find-other-file)))
```

LSP servers also provides diagnostics (lint, errors etc ...). **Flymake* is
responsible for working with those:

```elisp
(use-package flymake
  :bind (("C-c l a" . flymake-show-project-diagnostics)
         ("C-c l b" . flymake-show-buffer-diagnostics)
         ("C-x ]"   . flymake-goto-next-error)
         ("C-x ["   . flymake-goto-prev-error)))
```

**Flyspell** works with both text and programming modes:

```elisp
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (pro-mode . flyspell-prog-mode)))
```

Unfortunately, emacs doesn't ship with support for Rust or Go. There is no
getting around that, except downloading external packages.

```elisp
;; For rust. `rustic-enable-detached-file-support` so that LSP works with
;; files that do not belong to a cargo project.
(use-package rustic
  :hook (rustic-mode . eglot-ensure)
  :init
  (setq rustic-lsp-client 'eglot
        rustic-enable-detached-file-support t))
        
        
;; For Go. Originally taken from Centaur Emacs.        
(use-package go-mode
  :functions (go-packages-gopkgs go-update-tools)
  :hook (go-mode . eglot-ensure)
  :init
  (setq-default eglot-workspace-configuration
                '((:gopls . ((usePlaceholders . t)))))
  :config
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c t a" . go-test-current-project)
                ("C-c t m" . go-test-current-file)
                ("C-c t ." . go-test-current-test)
                ("C-c t x" . go-run))))        
```

Eglot doesn't install LSP servers by itself. You can automate it however. Here's
an example from Centaur Emacs:

```elisp
(defvar go--tools '("golang.org/x/tools/gopls"
                    "golang.org/x/tools/cmd/goimports"
                    "github.com/go-delve/delve/cmd/dlv"
                    "github.com/josharian/impl"
                    "github.com/cweill/gotests/..."
                    "github.com/davidrjenni/reftools/cmd/fillstruct"))

(defun go-install-tools ()
  "Install or update go tools."
  (interactive)
  (unless (executable-find "go")
    (user-error "Unable to find `go' in `exec-path'!"))

  (message "Installing go tools...")
  (let ((proc-name "go-tools")
        (proc-buffer "*Go Tools*"))
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process proc-name proc-buffer "go" "install" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status))))))))
```               


## Project and Version Control

Emacs ships with a pretty good project management package called `project`. A
famous unofficial alternative is `projectile`. However, project does most of
what I want. My favorite commands are `find-file-in-project-or-directory` bound
to `C-t`, `projectile-switch-buffer`, `projectile-find-regexp`. Type `C-c p ?`
to see list of default keybindings.

```elisp
(use-package project
  :bind (("C-c M-k" . project-kill-buffers)
         ("C-c m"   . project-compile)
         ("C-x f"   . find-file)
         ("C-t"     . find-file-in-project-or-directory))
  :bind-keymap ("C-c p" . project-prefix-map)
  :defines find-file-in-project-or-directory
  :custom
  (project-switch-commands
   '((project-find-file   "Find file")
     (project-find-regexp "Grep" ?h)))
  :config
  (defun find-file-in-project-or-directory ()
    (interactive)
    (if (project-current nil)
        (project-find-file)
      (call-interactively #'find-name-dired))))
```

Emacs also ships with a package for **Version Control** called **vc**. I've
tweaked it to show commit stats along with the message, and slightly different
formatting. Log view and blaming is what I like to use from this package.
Otherwise, I'm not a fan, and prefer [Magit](https://magit.vc) for anything
remotely non-trivial.

```elisp
(use-package vc
  :bind
  (("C-c i" . vc-git-grep)
   ("C-x g" . vc-dir-root))
  :config
  (defun vc-git-expanded-log-entry (revision)
    "Just adds commit stats to expanded commit entry in log-view."
    (with-temp-buffer
      (apply #'vc-git-command t nil nil
             (list "log" revision "-1" "--stat" "--no-color" "--stat" "--"))
      (goto-char (point-min))
      (unless (eobp)
        ;; Indent the expanded log entry.
        (while (re-search-forward "^" nil t)
          (replace-match "  ")
          (forward-line))
        (concat "\n" (buffer-string))))))
```

## Dired

Highly recommend it if you don't use it already. It works fairly well out of the
box. I've just unset the keymap for `image-dired` as I prefer that for finding
files in the current directory or project. It also prefers *GNU ls* if
available, as it has `--group-directories-first` option.

```elisp
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :hook ((dired-mode . (lambda () (local-unset-key (kbd "C-t"))))) ; image-dired
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies  'always
        dired-isearch-filenames 'dwim)
  (when (executable-find "gls")
    ;; Use GNU ls when possible.
    (setq dired-use-ls-dired nil)
    (setq ls-lisp-use-insert-directory-program t)
    (setq insert-directory-program "gls")
    (setq dired-listing-switches "-alh --group-directories-first"))
  (use-package dired-aux :ensure nil))
```

## Appearance and other tweaks

There are a bunch of random small tweaks that are not quite necessary, but
really good to have.

Control the size of frame:

```elisp
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 100)))
```

Disable some GUI features (also makes it load a bit faster):

```elisp
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
  
;; Go directly to scratch buffer.
(setq inhibit-splash-screen t)

(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(global-unset-key (kbd "<C-wheel-down>"))
(global-unset-key (kbd "<C-wheel-up>"))

;; Don't show native comp warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; Never kill scratch.
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
```

Change some other aspects:

```elisp
;; Show path if names are same
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Secondary click open context menu in GUI.
(context-menu-mode)

;; Ask yes or no, explicitely.
(fset 'yes-or-no-p 'y-or-n-p)

;; Menubar in terminal.
(bind-key "M-`" #'tmm-menubar)

;; Show column numbers in mode line.
(setq column-number-mode t)

;; Load remote environment when using tramp-mode.
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; Show a message instead of beeping.
(setq visible-bell t)
(setq ring-bell-function (lambda () (message "*woop*")))

;; Don’t compact font caches during GC. IIRC, improves performance.
(setq inhibit-compacting-font-caches t)

;; Some settings around text auto-filling. This is probably unnecessary.
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)
```

I never found the position of `Meta` key comfortable on Macs. This swaps `Super`
and `Meta/Option` key:

```elisp
;; OS specific tweaks
(cond
 ((string-equal system-type "darwin")
  (setq ns-use-thin-smoothing t)
  (setq mac-option-key-is-meta    t
        mac-command-key-is-meta   nil
        mac-command-modifier     'meta
        mac-option-modifier      'super

        ns-use-native-fullscreen  nil))
 ((string-equal system-type "gnu/linux") t))

```

Emacs 29 comes with much better scrolling called `pixel-scroll-precision-mode`.
Also, the default scrolling feels a bit odd to me, and these options make it
feel a bit more natural.

```elisp
(when (and window-system (>= emacs-major-version 29))
  (pixel-scroll-precision-mode 1))

(setq scroll-preserve-screen-position  'always
      mouse-wheel-scroll-amount        '(1 ((shift) . 1)) ; Scroll one line at a time.
      mouse-wheel-progressive-speed     nil
      scroll-step                       1
      scroll-margin                     1
      scroll-conservatively             10000)
```

Minor mode names can quickly clutter your modeline.
[diminish](https://github.com/emacsmirror/diminish) is great, but there is a DIY
solution that I found somewhere on Reddit:

```elisp
(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)
(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))
```


Emacs uses a dedicated buffer to show compilation output. You can set it to
scroll automatically while its still running the compile process. We also add a
hook to apply colors to the output, as some compilation programs produce
fancier outputs. It may or may not interfere with your grep-mode output.

```elisp
;; Compilation output, autoscroll
(setq compilation-scroll-output t)

(use-package ansi-color
  :config
  (defun --compilation-filter (fn proc string)
    "Wrap `compilation-filter' (FN PROC STRING) to support `ansi-color'."
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (funcall fn proc string)
          (when (not (equal major-mode 'grep-mode))
            (let ((inhibit-read-only t))
            (ansi-color-apply-on-region (point-min) (point-max))))))))
  (advice-add 'compilation-filter :around #'--compilation-filter))
```

Lastly, I've a dedicated file for some extra useful things that don't ship with
Emacs. Currently, I've got go-mode, rustic, winnow, persistent-scratch, popper
and corfu added to that file. **Winnow** is great for filtering compilation/grep
outputs. The default behavior around popups is very unpredictable, and
**Popper** forces those popup buffers to show up in the right place.

```elisp
;; For local configurations.
(load (expand-file-name "local.el" user-emacs-directory) 'noerror)
```

## Conclusion

That's it, we are done! I mainly use it as-is whenever I'm temporarily working
on a new server, container or a virtual machine. With some extra packages that I
mentioned earlier, I find myself using it more than my "more complete" `.emacs`.
I keep it updated [as a GitHub
Gist](https://gist.github.com/vishesh/8aa9dec6b0097b9c5e14688638831c6a), where
you can also leave comments.
