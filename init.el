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

;; DISABLE TRAMP MODE
(setq tramp-mode nil)

;; Access packages in .emacs.d
(add-to-list 'load-path "~/.emacs.d")

;; Remove clutter
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; Always use CommonLisp extensions
(require 'cl)

;; Paren highlighting
(show-paren-mode 1)
; (setq show-paren-style 'parenthesis) ; Highlight just parens
(setq show-paren-style 'expression) ; Highlight entire expression

;; Fix tabs
(let ((tab-size 4))
  (add-hook 'c-mode-hook
            (lambda () 
              (setq c-basic-offset tab-size)
              (setq c-indent-level tab-size)
              (setq tab-width tab-size)))
  (setq tab-width tab-size)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width tab-size)
  (setq-default python-indent tab-size)
  (setq-default py-indent-offset tab-size)
  (setq python-basic-offset tab-size)
  (setq python-guess-indent nil))

(add-hook 'python-mode-hook
		 (lambda () 
		   (setq indent-tabs-mode nil)
		   (setq tab-width (default-value 'tab-width))))

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
    (setq python-python-command "python2.6")

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

;; Hide emacs turds
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ORG-MODE
(when (require 'org-install nil t)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (define-key mode-specific-map [?a] 'org-agenda)
  (require 'remember)
  (add-hook 'remember-mode-hook 'org-remember-apply-template)
  (define-key global-map [(control meta ?r)] 'remember)
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
   org-remember-store-without-prompt t    ; Store notes in default place (makes it quicker)
   org-remember-templates '((116 "* TODO %?\n %u" "~/todo.org" "Tasks") ; Templates for quick tasks (C-M-r t)
                            (110 "* %u %?" "~/notes.org" "Notes"))      ;                 and notes (C-M-r n)
   remember-annotation-functions '(org-remember-annotation)
   remember-handler-functions '(org-remember-handler)))

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

(global-set-key [?\M-o] 'rot-windows)

;;; PYTHON
;; Allow loading of local packages in run-python
(setq python-remove-cwd-from-path nil)
;; Autoindent on return (\C-m seems to mirror enter)
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

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

;; Markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Add C-x p to be the opposite of C-x o
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key [(control ?x) ?p] 'prev-window)

;; Make Term sane
(require 'term)
(term-set-escape-char ?\C-x) ; Make C-x consistent inside term windows
(define-key term-raw-map "\C-c" 'term-interupt-subjob)

;; Replace yes with y
(fset 'yes-or-no-p 'y-or-n-p)


;; Auto-complete where available...
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)

  ;; Auto-complete-mode + cygwin-mount + network mounts = frozen emacs
  ;; Define cygwin-root earlier (in a single place)
  (let* ((cygwin-root "c:/cygwin"))
         (when (and (eq 'windows-nt system-type)
                    (file-readable-p cygwin-root))
           (setq ac-ignores (list "//"))))

  (setq ac-quick-help-delay 0.8)
  (setq ac-candidate-limit 20))

;; Someone kill the inventor of this
(when (= emacs-major-version 24)
  (set-message-beep 'silent))

;; Always show the column number
(setq column-number-mode t)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Configure ERC
(setq erc-nick "scombinator"
      erc-user-full-name "λ xyz.xz(yz)"
      erc-hide-list '("JOIN" "PART" "QUIT")) ; Don't notify on join/part/quit

;; Common Lisp Indentation rules != ELISP rules
(add-hook 'lisp-mode-hook
          (lambda () 
            (set (make-local-variable lisp-indent-function)
                 'common-lisp-indent-function)))


;; In linux (without w3m's override below) use chromium
(when (eq 'gnu/linux system-type)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium"))

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
      ido-everywhere t
      ido-auto-merge-delay-time 9999999) ; Prevents ido from deciding to look elsewhere
(ido-mode 1)

;; Don't interupt displaying for input.
(setq redisplay-dont-pause t)

;; Silliness - from #emacs
(defun is-this-for-that ()
  (interactive)
  (with-temp-buffer (url-insert-file-contents
                     "http://itsthisforthat.com/api.php?text") (buffer-substring
                                                                (point-min)(point-max))))

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

;; Make ediff saner, it's still bad
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Only seems to work on Emacs 24...
(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      (when (eq major-mode 'shell-mode)
                        (lambda ()
                          (let ((proc (get-buffer-process (current-buffer)))
                                (str (format "export COLUMNS=%s" (window-width))))
                            (when proc (funcall comint-input-sender proc str))))
                        nil t))))
          
;; Setup RSS feeds
;; Open newsticker with M-x newsticker-show-news
(setq newsticker-url-list '(("Stuff" "http://stuff.co.nz/rss/" nil nil nil)
                            ("M-x emacs-reddit" "http://reddit.com/r/emacs/.rss" nil nil nil)))
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

(el-get 'sync '(workgroups graphviz-dot-mode))

;; Use workgroups if available, otherwise try escreen ...
(if (require 'workgroups nil t)
    (progn 
      (global-set-key (kbd "C-\\") nil)
      (setq wg-prefix-key (kbd "C-\\")) ; Match escreen keybindings
      (workgroups-mode 1)
      (setq wg-morph-on nil))
  (when (require 'escreen nil t) ; C-\ c, C-\ n, C-\ p, C-\ k
    (escreen-install)))

;; Toggle between dark and night
(defun update-color-theme ()
  (if color-theme-is-dark
      (funcall color-theme-dark-theme)
      (funcall color-theme-light-theme)))

(defun toggle-color-theme ()
  (interactive)
  (setq color-theme-is-dark (not color-theme-is-dark))
  (update-color-theme))

;; system specific fonts
(when (eq 'windows-nt system-type)
    (set-default-font "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"))
(when (eq 'gnu/linux system-type)
    (set-default-font "-unknown-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

;; Make the font size reasonable
(set-face-attribute 'default nil :height 100)

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

;; Run a shell on startup
;; eshell impacts badly on run-python in emacs23
(if (= emacs-major-version 24)
  (eshell) 
  (shell))

(add-to-list 'load-path "~/.emacs.d/color-theme")
(global-font-lock-mode t)
(when (require 'color-theme nil t)
  (require 'color-theme-autoloads "color-theme-autoloads")
  (color-theme-initialize)
  (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
  (if (require 'color-theme-solarized nil t)
      (setq color-theme-dark-theme 'color-theme-solarized-dark
            color-theme-light-theme 'color-theme-solarized-light)
    (setq color-theme-dark-theme 'color-theme-charcoal-black
          color-theme-light-theme 'color-theme-vim-colors))
  (setq color-theme-is-dark nil
        color-theme-is-global t
        color-theme-is-cumulative t)
  (update-color-theme)) ; color-theme-is-cumulative appears to be buggy in init.
