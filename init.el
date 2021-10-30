;; 
;; On windows 
;; - Add /cygwin/c/PATH_TO_EMACS_ETC to $PYTHONPATH
;; - Add cygin-mount.el to .emacs.d
;; - Set HOME & WORKSPACE environment vars
;; - Set PYTHONPATH to the cygdrive path, ensuring it ends with a ':'
;; - Install gnutls and add to PATH
;;
;; On linux
;; - Install Inconsolata (or pick a different font)
;; - Change "chromium" to desired browser
;;

;; Disable tramp mode to prevent loading for completions (that effectively halt emacs to search the network)
(setq tramp-mode nil)

;; Access packages in .emacs.d
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Remove clutter
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; Bar cursor is correct style of cursor to use. Point is *between* characters.
(setq-default cursor-type 'bar)

(setq initial-scratch-message nil)
(setq indicate-empty-lines t)

;; Always use CommonLisp extensions
(require 'cl)

;; Paren highlighting
(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ; Highlight just parens
;(setq show-paren-style 'expression) ; Highlight entire expression

;; Fix tabs
(let ((tab-size 4))
  (add-hook 'c-initialization-hook
            (lambda ()
              (define-key c-mode-base-map "\C-m" 'c-context-line-break)))
  (add-hook 'c-mode-hook
            `(lambda ()
              (setq c-basic-offset ,tab-size
                    c-indent-level ,tab-size
                    tab-width ,tab-size)))
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))
  (setq-default tab-width tab-size)
  (setq c-basic-offset tab-size)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width tab-size)
  (setq-default python-indent tab-size)
  (setq-default py-indent-offset tab-size)
  (setq python-basic-offset tab-size)
  (setq python-guess-indent nil))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; PYTHON
;; Allow loading of local packages in run-python
(setq python-remove-cwd-from-path nil)
(add-hook 'python-mode-hook
          (lambda () 
            ;; Fix for archlinux
            (when (eq 'gnu/linux system-type)
              (setq python-command "python3"))
            (define-key python-mode-map "\C-m" 'newline-and-indent)
            (setq indent-tabs-mode nil
                  tab-width (default-value 'tab-width))))

;; If we're on a windows machine - setup Cygwin
(let* ((cygwin-root "c:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))

    ;; Add cygwin to path
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

    ;; Change shell to ZSH
    (setq shell-file-name "zsh")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)

    ;; Default dir to workspace
    (when (getenv "WORKSPACE")
      (setq default-directory (getenv "WORKSPACE")))

    ;; NTEmacs can't seem to follow the link in cygwin
    (setq python-python-command "python2.7")

    (setq eshell-force-execution t)

    (require 'cygwin-mount)
    (cygwin-mount-activate)))

;; Slime setup on linux
(when (eq 'gnu/linux system-type)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
  (when (require 'slime nil t)
    (slime-setup '(slime-fancy slime-asdf))
    (setq common-lisp-hyperspec-root "/usr/share/doc/HyperSpec/")))

;; USE RUBY MODE
(autoload 'ruby-mode "ruby-mode"
  "Mode for editting ruby code" t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(setq interpreter-mode-alist 
      (append '(("ruby" . ruby-mode)) 
              interpreter-mode-alist))

(autoload 'run-ruby "inf-ruby")
(autoload 'inf-ruby-keys "inf-ruby")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))

;; Provide LEX & YACC 'MODES'
(add-to-list 'auto-mode-alist '("\\.l\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . fundamental-mode))

;; Use js-mode for json
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

;; Use XML mode for XSLT
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))

;; Hide emacs turds
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;; Makefiles need hard TABS
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq-default indent-tabs-mode t)
            (setq tab-width 4)))
(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq-default indent-tabs-mode t)
            (setq tab-width 4)))            
(add-hook 'before-save-hook
          (lambda ()
            (if (member major-mode '(makefile-mode makefile-gmake-mode))
                (tabify (point-min) (point-max)))))

;; ORG-MODE
(when (require 'org-install nil t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (define-key mode-specific-map [?a] 'org-agenda)
  (require 'remember)
  (add-hook 'remember-mode-hook 'org-remember-apply-template)
  (define-key global-map [(control meta ?r)] 'remember)
  (require 'org-publish nil t)
  (setq
   org-agenda-files '("~/todo.org")       ; List of files where todo items can be found:
   org-default-notes-file "~/notes.org"   ; File to store simple notes (used by remember)
   org-agenda-ndays 7                     ; Days the default agenda view should look ahead
   org-deadline-warning-days 14           ; Days early you are warned of impending deadlines
   org-agenda-show-all-dates t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-start-on-weekday nil
   org-reverse-note-order t
   org-startup-indented t
   org-export-with-sub-superscripts '{}   ; Always require braces around super/sub-scripts
   org-remember-store-without-prompt t    ; Store notes in default place (makes it quicker)
   org-remember-templates '((116 "* TODO %?\n %u" "~/todo.org" "Tasks") ; Templates for quick tasks (C-M-r t)
                            (110 "* %u %?" "~/notes.org" "Notes"))      ;                 and notes (C-M-r n)
   remember-annotation-functions '(org-remember-annotation)
   remember-handler-functions '(org-remember-handler))
  ;(org-defkey org-mode-map (kbd "C-c n") 'my-org-table-swap-next-cell)
  ;(org-defkey org-mode-map (kbd "C-c p") 'my-org-table-swap-prev-cell)
  (setq org-export-html-style
"<style type=\"text/css\">
* { margin: 0; padding: 0; }
html, body { height: 100%; }
h1, h2, h3, h4, h5, h6 { font-size: 18px; font-family: sans-serif; font-weight: normal; }
h1, h2, h3 { letter-spacing: -1px; font-weight: bold; }
h1, h2 { text-align: center; font-size: 24px; }
h1 { margin-bottom: 1em; font-size: 32px; font-weight: bolder; }
.title { font-size: 36px; }
#content { text-align: justify; width: 52em; margin: 3em auto 2em auto; line-height: 1.5em; }
.outline-1 { margin-bottom: 2em; }
.outline-2 { margin-bottom: 1em; }
.outline-3 { margin-bottom: 1em; }
.outline-4 { margin-bottom: 1em; padding-left: 2em; }
.outline-text-3 { padding-left: 2em; }
.outline-text-4 { padding-left: 2em; }
.done, .todo { font-weight: bold; letter-spacing: -1px; }
li { margin-left: 1em; }
table { border-collapase: collapse; margin: 0 auto; }
table, th, td { border: 1px solid white; border-left: 8px solid white; border-right: 8px solid white; }
td { padding: 4px; }
th { background-color: #F90; }
tr:nth-child(2n) { background-color: #FF8; }
</style>"))

;; Add function to swap orgmode table field with next/prev field
(defun my-org-table-swap-next-cell ()
  (interactive)
  (let ((org-table-clip nil))
    (org-table-cut-region (point) (point))
    (let ((first-clip org-table-clip))
      (org-table-next-field)
      (org-table-cut-region (point) (point))
      (let ((second-clip org-table-clip)
            (org-table-clip first-clip))
        (org-table-paste-rectangle)
        (org-table-previous-field)
        (let ((org-table-clip second-clip))
          (org-table-paste-rectangle)
          (org-table-next-field))))))

(defun my-org-table-swap-prev-cell ()
  (interactive)
  (let ((org-table-clip nil))
    (org-table-cut-region (point) (point))
    (let ((first-clip org-table-clip))
      (org-table-previous-field)
      (org-table-cut-region (point) (point))
      (let ((second-clip org-table-clip)
            (org-table-clip first-clip))
        (org-table-paste-rectangle)
        (org-table-next-field)
        (let ((org-table-clip second-clip))
          (org-table-paste-rectangle)
          (org-table-previous-field))))))

;; Make #! scripts executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Make windows rotate on Alt-o
(defun rot-windows ()
  "Rotate all the windows."
  (interactive) 
  (defun rot (list)
    (append (cdr list) (list (car list))))
  (let* ((w-l (window-list))
         (rb-l (rot (mapcar 'window-buffer w-l)))
         (rs-l (rot (mapcar 'window-start w-l))))
    (mapcar* 'set-window-buffer w-l rb-l)
    (mapcar* 'set-window-start w-l rs-l)
    nil))

;; TODO: trim *...* buffers - limit it to just buffers with files...
(defun list-modified-buffers ()
  "List modified buffers."
  (interactive)
  (with-output-to-temp-buffer "*modified-buffers*"
    (print 
     (remove-if-not 'buffer-modified-p (buffer-list)))))

(global-set-key [?\M-o] 'rot-windows)

;; I find myself doing this by habit, so also make vim work in eshell
(defun eshell/vim (&rest args)
  "Invoke `find-file' on the file.
    \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))
(defun eshell/vi (&rest args)
  (apply 'eshell/vim args))

;; Add C-x p to be the opposite of C-x o
(defun prev-window()
  (interactive)
  (other-window -1 t))
;; These lambdas allow for the use of multiple frames via C-x 5 2
(global-set-key [(control ?x) ?p] (lambda () (interactive) (other-window -1 t) (raise-frame)))
(global-set-key [(control ?x) ?o] (lambda () (interactive) (other-window 1 t) (raise-frame)))

;; Make Term sane
(eval-after-load 'term
  '(progn (term-set-escape-char ?\C-x) ; Make C-x consistent inside term windows
          (define-key term-raw-map "\C-c" 'term-interupt-subjob)))

;; Replace yes with y
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-complete where available...
(when (require 'auto-complete-config nil t)

  (define-key ac-completing-map [return] nil)
  (define-key ac-completing-map "\r" nil)
  
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)

  (define-key ac-completing-map [return] nil)
  (define-key ac-completing-map "\r" nil)

  ;; Auto-complete-mode + cygwin-mount + network mounts = frozen emacs
  ;; Define cygwin-root earlier (in a single place)
  (let* ((cygwin-root "c:/cygwin"))
         (when (and (eq 'windows-nt system-type)
                    (file-readable-p cygwin-root))
           ; These ignores prevent searching around the network.
           (setq ac-ignores (list "//" "///" "////" "/////"))))

  (setq ac-quick-help-delay 0.8
        ac-candidate-limit 20)

  ;; Stop stealing RETURN, (use C-RET / M-RET instead)
  ;; This is irritating when it completes and you want to get on with the next line
  (define-key ac-complete-mode-map "\r" nil)
  (define-key ac-complete-mode-map [return] nil)
  
  (if (eq window-system nil)
      (define-key ac-complete-mode-map (kbd "<M-return>") 'ac-complete)
    (define-key ac-complete-mode-map (kbd "<C-return>") 'ac-complete))

  (define-key ac-completing-map [return] nil)
  (define-key ac-completing-map "\r" nil))


;; Why would anyone want it to beep - seriously?
(when (fboundp 'set-message-beep)
  (set-message-beep 'silent))

;; Always show the column number
(setq column-number-mode t)

;; UTF-8
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; Configure ERC
(setq erc-nick "scombinator"
      erc-user-full-name "λ xyz.xz(yz)"
      erc-hide-list '("JOIN" "PART" "QUIT") ; Don't notify on join/part/quit
      erc-server-auto-reconnect nil) ; auto-reconnect disobeys options and locks emacs if no connection is available

;; Common Lisp Indentation rules != ELISP rules
(add-hook 'lisp-mode-hook
          (lambda () 
            (set (make-local-variable lisp-indent-function)
                 'common-lisp-indent-function)))


;; In linux (without w3m's override below) use a good browser
(when (eq 'gnu/linux system-type)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox"))

;; Start w3m when available
;; W3M keys: 
;; C-c C-t => new tab
;; C-c C-w => close tab
;; C-c C-p => prev tab
;; C-c C-n => next tab
;; C-c C-s => tab list
(when (require 'w3m-load nil t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-default-display-inline-images t))

;; Open links with C-x g, open arbitrary urls with C-x u 
(global-set-key [(control ?x) ?u] 'browse-url)
(global-set-key [(control ?x) ?g] 'browse-url-at-point)

;; C-z is annoying/dangerous as is... Make it match other programs
(global-set-key (kbd "C-z") 'undo)
;; M-ESC ESC or ESC ESC ESC is the cause of mysteriously disappearing buffers
(global-set-key (kbd "M-ESC ESC") nil)

;; IDO makes changing buffers nicer
(setq ido-enable-flex-matching t
      ido-enable-tramp-completion nil    ; Tramp is slow to require, and has a noticable pause
      ido-auto-merge-delay-time 9999999) ; Prevents ido from deciding to look elsewhere
(ido-mode 1)
(ido-everywhere t)

;; Don't interupt displaying for input.
(setq redisplay-dont-pause t)

;; SQLITE DBS do not have an extension!
(setq sql-sqlite-login-params '((database :file "([^\\.]*\\|.*\\.\\(db\\|sqlite[23]?\\)\\)")))

;; internal '.'s point to a url, filename or number
;; May be able to normalise case (afterwards??? - when we have all the counts)
(defun language-model-current-buffer ()
  "Currently creates a TF-IDF language model of the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((language-model (make-hash-table :test 'equal))
          (word-count 0)
          (pmax (point-max)))

      (while (re-search-forward "[A-Za-z0-9'_.-]*[A-Za-z0-9_]"  ;; Must have one non (.'-) and these do not go at the end.
                                pmax t)
        (let ((word (match-string 0)))
          (if (gethash word language-model)
              (puthash word (1+ (gethash word language-model)) language-model)
            (puthash word 1 language-model))
          (incf word-count)))

      (with-output-to-temp-buffer "*Language Model*"
        (maphash (lambda (k v)
                   (setq result 
                         (princ (format "%s -> %s\n" 
                                        k 
                                        (log (/ word-count v)))))) ;; IDF - multiply this by TF of the line
                 language-model)))))

;; Differentiate between buffers with the same name by their path
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-strip-common-suffix t)

;; Make ediff saner, it's still very very bad
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Only seems to work on Emacs 24...
(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      (lambda ()
                        (when (derived-mode-p 'comint-mode)
                          (set-process-window-size (get-buffer-process (current-buffer))
                                                   (window-height)
                                                   (window-width))))
                      nil t)))

;; Setup RSS feeds
;; Open newsticker with M-x newsticker-show-news
(setq newsticker-url-list '(("M-x emacs-reddit" "http://reddit.com/r/emacs/.rss" nil nil nil)))
(setq newsticker-frontend 'newsticker-plainview)

;; Use el-get, Downloading it if needed, (ensure gnutls is installed in windows)
;; May need to do by hand in windows.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
                (lambda (s) 
                  (let (el-get-master-branch) 
                    (end-of-buffer) 
                    (eval-print-last-sexp)))))
(setq el-get-git-shallow-clone t)

(setq el-get-sources
      '((:name restclient :type git :url "https://github.com/pashky/restclient.el.git")))

(el-get 'sync '(markdown-mode
                anaphora
                color-theme
                ;color-theme-solarized
                ;haskell-mode
                mode-compile
                ;erlang-mode
                yaml-mode
                ;tuareg-mode
                ;rust-mode
                ;go-mode
                ;restclient
                ;nxhtml
                ))

;; Associate .http with RESTclient
(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))

;; Markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;; system specific fonts
(when (eq 'windows-nt system-type)
    (set-frame-font "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1" t t))
(when (eq 'gnu/linux system-type)
  ;(set-frame-font "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" t t))
  ;(set-frame-font "-PfEd-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" t t))
  (set-frame-font "-PfEd-Ricty Diminished-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"  t t))

;; Make the font size reasonable
(set-face-attribute 'default nil :height 120)

;; Rid us of the disabled C-x C-n command, if I want to use that then M-x is fine
(global-set-key (kbd "C-x C-n") nil)

;; Bind f1 and S-f1 to keyboard macro commands
(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)] 'toggle-kbd-macro-recording-on)

(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys) 'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys) 'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; Bind f5 to revert-buffer-with-modified-check, and toggle auto-revert with C-f5
(global-set-key '[(f5)] 
                (lambda () 
                  "Revert the current buffer, prompting if modified."
                  (interactive)
                  (revert-buffer t (not (buffer-modified-p)) t)))
(global-set-key '[(ctrl f5)] 'auto-revert-mode)

;; Prevent compilation buffer from showing up
(defadvice compile (around compile/save-window-excursion first () activate)
  (save-window-excursion ad-do-it))

(setq compilation-scroll-output 'first-error
      compilation-ask-about-save nil
      compilation-save-buffers-predicate '(lambda () nil))

(global-font-lock-mode t)

(global-set-key '[(ctrl %)] 'query-replace-regexp)
(setq glasses-separate-parentheses-p nil) ; Do not separate parens in glasses-mode

;; Deprecating flet was a pointlessly bad and stupid decision, make emacs shut its hole about it
(defalias 'flet 'cl-flet)

;; I have never ever wanted mode-compile to ask for arguments
(defun mode-compile-quiet ()
  (interactive)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest args) "")))
    (mode-compile)))

;; C-c C-% will set a buffer local hook to use mode-compile after saving the file
(global-set-key '[(ctrl c) (ctrl %)]
                (lambda () 
                  (interactive)
                  (if (member 'mode-compile-quiet after-save-hook)
                      (progn
                        (setq after-save-hook (remove 'mode-compile-quiet after-save-hook))
                        (message "No longer compiling after saving."))
                    (progn
                      (add-to-list 'after-save-hook 'mode-compile-quiet)
                      (message "Compiling after saving.")))))

(defvar rust-command "cargo" "command to compile rust")
(defvar rust-flags "check" "flags to compile rust")
(defvar rust-compilation-error-regexp-alist
  '(("^error[^:]*:" nil))
  "Alist that specifies how to match errors in rust output.
See variable compilation-error-regexp-alist for more details.")
(defun rust-compile ()
  "Run `rust-command' with `rust-flags' on current-buffer (`rust-mode')."
  (mc--shell-compile rust-command rust-flags rust-compilation-error-regexp-alist))


(defvar lisp-command "sbcl" "command to compile lisp")
(defvar lisp-flags "" "flags to compile lisp")
(defvar lisp-compilation-error-regexp-alist
  '(("^\\*\\*\\* - .*" nil))
  "Alist that specifies how to match errors in lisp output.
See variable compilation-error-regexp-alist for more details.")

(when (eq 'windows-nt system-type)
  (setq lisp-command "clisp"))

(defun lisp-compile ()
  "Run `lisp-command' with `lisp-flags' on current-buffer (`lisp-mode')."
  (mc--shell-compile lisp-command lisp-flags lisp-compilation-error-regexp-alist))

(defvar r-command "Rscript" "command to run R")
(defvar r-flags "" "flags to run R")
(defvar r-compilation-error-regexp-alist
  '(("^Error: .*" nil))
  "Alist that specifies how to match errors in R output.
See variable compilation-error-regexp-alist for more details.")

(defun r-compile ()
  "Run `r-command' with `r-flags' on current-buffer (`ess-mode')."
  (mc--shell-compile r-command r-flags r-compilation-error-regexp-alist))

(defvar go-command "go" "command to run go")
(defvar go-flags "run" "flags to run go")
(defvar go-compilation-error-regexp-alist
  '(("^\\(.+\\):\\([[:digit:]]+\\): \\(.+\\): .*$" 1 2 nil 1))
  "Alist that specifies how to match errors in go-compile output.
See variable compilation-error-regexp-alist for more details.")
(defun go-compile ()
  "Run `go-command' with `go-flags' on current-buffer (`go-mode')."
  (mc--shell-compile go-command go-flags go-compilation-error-regexp-alist))

(eval-after-load 'mode-compile
  '(setq mode-compile-modes-alist
         (append  '((lisp-mode . (lisp-compile kill-compilation))
                    (rust-mode . (rust-compile kill-compilation))
                    (ess-mode . (r-compile kill-compilation))
                    (erlang-mode . (erlang-compile kill-compilation))
                    (go-mode . (go-compile kill-compilation)))
                  mode-compile-modes-alist)))

(global-set-key '[(ctrl c) (c)] 'mode-compile-quiet)
(global-set-key '[(ctrl c) (k)] 'mode-compile-kill)

;; Name compilation buffer after the buffer name
(setq compilation-buffer-name-function 
      (lambda (mode)
          (concat "*" (downcase mode) ": " (buffer-name) "*")))

;; Don't if we're running rust-check on an entire project
(add-hook 'rust-mode-hook
          (lambda ()
            (set (make-local-variable 'compilation-buffer-name-function)
                 (lambda (mode)
                   (concat "*" (downcase mode) "*")))))


;; Search forward/backward for symbol at point
(when (require 'smart-symbol nil t)
  (global-set-key '[(meta n)] 'smart-symbol-go-forward)
  (global-set-key '[(meta p)] 'smart-symbol-go-backward))

;; Next buffer with the same mode
(defun next-same-mode ()
  (interactive)
  (let ((mode major-mode)
        (buffer-list (buffer-list)))
    (switch-to-buffer
     (or
      (first (member-if (lambda (buffer)
                          (eq (buffer-local-value 'major-mode buffer) mode))
                        (append (rest buffer-list) (list (first buffer-list)))))))))

(global-set-key '[(ctrl c) (b)] 'next-same-mode)

;; Mostly to prevent M-! from using the minibuffer if output is small.
(setf max-mini-window-height 0.1)

;; Useful for Keyboard macros
(defun increment-number-at-point (&optional arg)
  "Increment number at point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let ((inc-by (or arg 1))
             answer)
        (skip-chars-backward "0123456789")
        (skip-chars-backward "-" (- (point) 1))
        (or (looking-at "-?[0-9]+")
            (error "No number at point"))
        (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
        (replace-match (format "%d" answer))))))

(defun 2* (n) 
  "Helper, just as useful as 1+"
  (* 2 n))

(when (require 'ess-site nil t)
  (setq inferior-R-program-name "R"))

(setq minibuffer-prompt-properties
       (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

(auto-image-file-mode 1)

(iimage-mode)

(defun refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t))

(add-to-list 'compilation-finish-functions 
             (lambda (buffer msg)
               (save-excursion
                 (set-buffer buffer)
                 (refresh-iimages))))

(eval-after-load 're-builder
  '(setq reb-re-syntax 'string))

;; Workaround annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

(setq scroll-step 1)
(setq scroll-conservatively 101)
(setq auto-window-vscroll nil)
(setq transient-mark-mode t)


;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; TODO - bind to a real key
(global-set-key [?\C-=] 'toggle-window-dedicated)

(defun increment-region-indent ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) tab-width))
(defun decrement-region-indent ()
  (interactive)
  (indent-rigidly (region-beginning) (region-end) (- tab-width)))

(global-set-key [?\C-,] 'decrement-region-indent)
(global-set-key [?\C-.] 'increment-region-indent)


; M-x package-refresh-contents RET
; M-x package-install RET ein RET


(require 'package)
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq custom-file (concat user-emacs-directory "/custom.el"))

; pyvenv-activate to use virtual environments for python (i.e. when using compile mode)
(dolist (package '(use-package
                    ein
                    jedi
                    pyvenv
                    flycheck
                    solarized-theme
                    transpose-frame
                    powerline
                    terraform-mode
                    rust-mode
                    annotate
                    lsp-java
                    magit))
   (unless (package-installed-p package)
     (package-install package)))

(require 'powerline)
(powerline-default-theme)


(add-hook 'python-mode-hook 'jedi:setup)
; (setq jedi:complete-on-dot t)

(global-set-key [?\C-x ?4] 'transpose-frame)

(setq ein:output-area-inlined-images t)

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "s-."))
                        (lsp-enable-which-key-integration))))
  :config
  (define-key lsp-mode-map (kbd "s-.") lsp-command-map))

;; JAVA LSP
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
(setq lsp-java-format-settings-profile "GoogleStyle")

;; trying this
(setq lsp-rust-server 'rust-analyzer)
;; run "M-x lsp" in a rust buffer
;; (Optionally) bind commands like
;; lsp-rust-analyzer-join-lines, lsp-extend-selection and lsp-rust-analyzer-expand-macro to keys.

;; On Linux to install the rust-analyzer binary into ~/.local/bin, this commands could be used

;; $ curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer
;; $ chmod +x ~/.local/bin/rust-analyzer

;; Ensure ~/.local/bin is listed in the $PATH variable.


; Enable fly spell for text modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

; Disable fly spell for particular text modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))



;; Fix for changes to solarized changing API
(defun set-solarized-light ()
  (customize-set-variable 'frame-background-mode 'light)
  (load-theme 'solarized-light-high-contrast t))

(defun set-solarized-dark ()
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized-dark-high-contrast t))

;; Toggle between dark and night
(defun update-color-theme ()
  (if color-theme-is-dark
      (funcall color-theme-dark-theme)
    (funcall color-theme-light-theme)))

(defun toggle-color-theme ()
  (interactive)
  (setq color-theme-is-dark (not color-theme-is-dark))
  (update-color-theme))

(when (require 'color-theme nil t)
  (require 'color-theme-autoloads "color-theme-autoloads")
  (color-theme-initialize)
  (if (require 'color-theme-solarized nil t)
      (setq color-theme-dark-theme 'set-solarized-dark
            color-theme-light-theme 'set-solarized-light)
    (setq color-theme-dark-theme 'color-theme-charcoal-black
          color-theme-light-theme 'color-theme-vim-colors))
  (setq color-theme-is-dark nil
        color-theme-is-global t
        color-theme-is-cumulative t)
  (update-color-theme)) ; color-theme-is-cumulative appears to be buggy in init.

(toggle-color-theme)

(global-set-key [f5] 'revert-buffer)
