---
layout:     post
title:      "A lean and minimal Emacs configuration"
date:       2023-01-25 08:23:25 PST
categories: emacs editors
---

*Note: I may or may not be the original author of various scripts in this post.
Since, I've had these these scripts over several years, I honestly don't
remember the history/source of each. If its not me, its probably from EmacsWiki
or Reddit.*

Emacs is an amazing text editor for almost anything. However, it takes time and
experimentation to make it feel more productive than traditional options out
there (Visual Studio Code, Sublime, IntelliJ, Google Docs ...). Often, we end up
with giant configurations with dozens of scripts and external packages. They are
great, practical and productive, but also slow to install and start.

This got me thinking if I can trim down my configuration into a single `.emacs`
file, that only depends on builtin/shipped packages. Turns out, you can! And you
don't really loose much. Particularly, if you are using upcoming Emacs 29. In
this post, I will go through a lean, minimal and productive single file `.emacs`
configuration. It is tailored for software development, and won't use any
external packages on Emacs 29. You can find the complete configuration [over
here](https://gist.github.com/vishesh/8aa9dec6b0097b9c5e14688638831c6a).

First, we will configure the package manager and
[`use-package`](https://github.com/jwiegley/use-package) (ships with Emacs 29,
but also available in ELPA). Highly recommended if you are not using it already.
Its a very [standard
configuration](https://gist.github.com/vishesh/8aa9dec6b0097b9c5e14688638831c6a#file-emacs-L21-L51).

## Basic text editing

For general editing, I like to set these defaults:

```elisp
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

Next we change the behavior some basic keybindings for moving around. The names
implies what it does. 

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

Next, we add **rectangle-mode** which I primarily use it for adding or deleting text
from multiple lines. Alternatively,
[iedit-mode](https://github.com/victorhge/iedit) and
[multiple-cursors](https://github.com/magnars/multiple-cursors.el) are excellent
unofficial choices.

```elisp
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))
```

**Goto Address mode** identifies URLs and make them clickable.

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

I recommend to use this with **icomplete/fido** completion modes, which we'll
setup later in this post. **Project** and **Version Control (VC)** package come
with few convenient `grep` commands of its own.

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

Unlike most IDEs and many text editor, Emacs GUI is very minimal. This is great,
as there is even more screen real-estate to split windows. Standard keybindings
`C-x {2, 3, 1, 0}` work great for creating splits. I have these additional
keybindings to move around. Since, I use them a lot more than creating splits, I
selected keybindings that are easy to repeat.

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
navigate symbols. In vanilla emacs, we can instead use `highlight` package
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

## Programming

## Project and Version Control

## Dired

## Appearance and other tweaks

```
(bind-key "C-6"     'global-display-line-numbers-mode)
(bind-keys ("C-M->" . indent-rigidly-right)
           ("C-M-<" . indent-rigidly-left))
(bind-key "M-\\"    'imenu)

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
**Save History** saves the minibuffer history.
```
