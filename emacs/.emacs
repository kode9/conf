;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file                                                    ;;
;; Pierre-Luc Perrier <pluc@the-pluc.net>                                      ;;
;;                                                                             ;;
;; Copy this in your home and custom user.el                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; conf path
(add-to-list 'load-path '"~/conf/emacs/")
(add-to-list 'load-path '"~/.emacs.d/")

(load-file "~/conf/emacs/user.el")

;; Custom variables
(custom-set-variables
 '(tool-bar-mode nil)                   ; Turn off the toolbar
 '(menu-bar-mode nil)                   ; Turn off the menubar
 '(scroll-bar-mode nil)                 ; Turn off the scrollbar
 '(display-time-mode nil)               ; Do not show date and time
 '(show-paren-mode t)                   ; Highlight parenthesis
 '(mouse-avoidance-mode 'cat-and-mouse) ; Avoid cursor
 '(display-time-mode nil)               ; Hide time in mode-line
 '(line-number-mode t)                  ; Show line number in mode-line
 '(column-number-mode t)                ; Show column number in mode-line
 '(size-indication-mode t)              ; Show buffer size in mode-line
 '(display-battery-mode nil)            ; Hide battery information in mode-line
 '(show-trailing-whitespace t)          ; Trailing whitespaces
 '(python-python-command "python3")     ; Use python3 by default
 '(winner-mode t)                       ; Used for autoclose
 '(display-time-day-and-date)
 '(display-time-24hr-format)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 )

(setq
 inhibit-startup-screen t           ; Do not show the welcome message
 initial-scratch-message nil
 message-log-max 50                 ; Disable off message buffer
 visible-bell nil                   ; Get rid of bells
 ring-bell-function 'ignore
 frame-title-format "Emacs : %f"    ; Frame title format
 savehist-file "~/.emacs.d/history" ; History file
 vc-follow-symlinks t               ; Automatically follow symlinks to files under CVS
 )

;; Save history beetween sessions
(savehist-mode t)

;; Start emacs-client server
(server-start)

;; Package archive
(require 'package)
;;; Additional packages http://marmalade-repo.org/
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Theme
(add-to-list 'load-path '"~/.emacs.d/elpa/color-theme-20080305.34/")
;;; Zenburn color theme
(require 'zenburn)
(color-theme-zenburn)
;;; Solarized
;; (add-to-list 'custom-theme-load-path '"~/usr/src/emacs-color-theme-solarized/")
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

;; Identify multiple buffers with the same file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Show region size and maximum column number in mode-line
;;; http://www.emacswiki.org/emacs/modeline-posn.el
;;; Does not work with powerline
(add-to-list 'load-path '"~/.emacs.d/elpa/modeline-posn-20131227.140/")
(add-to-list 'load-path '"~/.emacs.d/elpa/modeline-posn-20140121.553/")
(require 'modeline-posn)
(set 'modelinepos-column-limit 100)

(defun powerline-pluc-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (funcall separator-left face1 face2)
                                     (powerline-raw "%l ϟ %c" face2 'l)
                                     (funcall separator-left face2 face1)
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

;; Powerline (custom mode-line)
;;; https://github.com/milkypostman/powerline
(add-to-list 'load-path '"~/.emacs.d/elpa/powerline-20131126.1817/")
(add-to-list 'load-path '"~/.emacs.d/elpa/powerline-20140516.2128/")
(require 'powerline)
(powerline-pluc-theme)
;; (powerline-default-theme)

;; http://www.emacswiki.org/emacs/AutoIndentMode
(add-to-list 'load-path '"~/.emacs.d/elpa/auto-indent-mode-20131220.1220/")
(add-to-list 'load-path '"~/.emacs.d/elpa/auto-indent-mode-20140505.655/")
(setq auto-indent-on-visit-file t)
(require 'auto-indent-mode)
(auto-indent-global-mode)
;; https://github.com/pmarinov/clean-aindent
(add-to-list 'load-path '"~/conf/emacs/clean-aindent")
(require 'clean-aindent)

;; View, stage and revert Git changes straight from the buffer.
;;; https://github.com/nonsequitur/git-gutter-plus
(add-to-list 'load-path '"~/.emacs.d/elpa/git-commit-mode-20131230.729")
(add-to-list 'load-path '"~/.emacs.d/elpa/git-commit-mode-20140313.1504")
(add-to-list 'load-path '"~/.emacs.d/elpa/git-commit-mode-20140605.520")
(add-to-list 'load-path '"~/.emacs.d/elpa/git-gutter+-20130918.435")
(add-to-list 'load-path '"~/.emacs.d/elpa/git-gutter+-20140205.802")
(require 'git-gutter+)
(global-git-gutter+-mode t)

;; Autoscoll compilation buffer and stop on first error
(set 'compilation-scroll-output 'first-error)
;; Skip warnings when jumping between errors
(set 'compilation-skip-threshold 2)

;; Highlights current line in compilation within another buffer
;;; Actually it's a bit annoying...
;; (add-to-list 'load-path '"~/.emacs.d/elpa/fm-20130612.1")
;; (require 'fm)
;; (add-hook 'compilation-mode-hook (lambda () (fm-start)))
;; (remove-hook 'compilation-mode-hook (lambda () (fm-start)))

;; Shorten long file-name targets
;;; https://github.com/lewang/scf-mode
;;; Seems to work in grep, but not in compile :(
(add-to-list 'load-path '"~/.emacs.d/elpa/scf-mode-20111202.707/")
(autoload 'scf-mode "scf-mode" "SCF Mode" t)
(add-hook 'compilation-mode-hook (lambda () (scf-mode t)))

;; Stop asking yes/no before compile when a compilation is already running
;;; ftp://download.tuxfamily.org/user42/compilation-always-kill.el
(autoload 'compilation-always-kill-mode "compilation-always-kill" nil t)
(compilation-always-kill-mode t)

;; Major modes
;;; Path
(add-to-list 'load-path '"~/conf/emacs/qml-mode")
(autoload 'glsl-mode "glsl-mode" nil t)
;;; Load
(autoload 'qml-mode "qml-mode" "QML mode" t)
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'cmake-mode "cmake-mode" "Cmake Mode." t)
(autoload 'cuda-mode "cuda-mode" "Cuda Mode." t)
;;;; Filename patterns
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\|mdwn\\|mdml\\)\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.\\(sci\\|sce\\)\\'" . scilab-mode))
(add-to-list 'auto-mode-alist '("\\.axl\\'" . xml-mode)) ; Axel modeler
(add-to-list 'auto-mode-alist '("\\.F90\\'" . f90-mode))
(add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.xrdb\\'" . conf-xdefaults-mode))
(add-to-list 'auto-mode-alist '(".*_EDITMSG\\'" . log-entry-mode)) ; Git commit
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|geom\\|vs\\|fs\\|gs\\)\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

;; Minor modes
(autoload 'align-string "align-string" "Align string." t)
;;; Insert operators with surrounding spaces smartly
(autoload 'smart-operator "smart-operator-mode" "Smart operator." t)
;;; https://github.com/alamaison/emacs-cmake-project
(autoload 'cmake-project-mode "cmake-project")
(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)
;;; Better SQL indentation
(eval-after-load "sql" (load-library "sql-indent"))
;;; Yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet")
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0")
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20131224.143")
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20140514.1649")
(require 'yasnippet)
(yas-global-mode 1)
;;; Autocomplete
;; ;;;; Andy Stewart init
;; (require 'init-auto-complete)
;; ; default init
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

;; ; clang
;; (require 'auto-complete-clang)
;; (setq ac-quick-help-delay 0.8)
;; ;; (ac-set-trigger-key "TAB")
;; ;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
;; ; (define-key ac-mode-map  [(control tab)] 'auto-complete)

;; (add-to-list 'ac-sources 'ac-source-dictionary)
;; (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;; (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;; (add-hook 'css-mode-hook 'ac-css-mode-setup)
;; (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
;; (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

;; ;; http://www.cx4a.org/pub/auto-complete-config.el
;; ;; http://idning.googlecode.com/svn/trunk/config/.emacs.d/auto-install/auto-complete-config.el

;; (defmacro ac-define-dictionary-source (name list)
;;   "Define dictionary source named `NAME'.
;; `LIST' is a list of string.
;; This is useful if you just want to define a dictionary/keywords source."
;;   `(defvar ,name
;;      '((candidates . (list ,@list))
;;        (cache))))

;; (ac-define-dictionary-source
;;  ac-source-c++-keywords
;;  '("and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
;;    "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
;;    "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
;;    "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected"
;;    "signed" "template" "typeid" "void" "auto" "catch" "continue" "else"
;;    "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
;;    "bitand" "char" "default" "enum" "for" "long" "operator" "register"
;;    "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
;;    "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true"
;;    "unsigned" "while" "nullptr" "constexpr"))

;; (defun ac-c++-keywords-setup ()
;;   (push 'ac-source-c++-keywords ac-sources))

;; (defun ac-c++-keywords-initialize ()
;;   (add-hook 'c++-mode-hook 'ac-c++-keywords-setup)
;;   t)

;; ;; latex https://bitbucket.org/tequilasunset/auto-complete-latex/
;; (require 'auto-complete-latex)

;; (global-auto-complete-mode t)

;; Custom hooks
(defun clean-buffer()
  "Remove trailing whitespaces and tabs"
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

(defun iwb()
  "indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun dev-hooks()
  "Hooks for dev files"
  (set-buffer-file-coding-system 'utf-8-unix)
  (clean-buffer)
  (copyright-update))

(defun add-local-dev-hooks()
  "See dev-hooks"
  (add-hook 'before-save-hook 'iwb nil t))

;;; Clean for any files
(add-hook 'before-save-hook 'clean-buffer)
;;; For dev
(add-hook 'c-mode-common-hook 'add-local-dev-hooks)
(add-hook 'cmake-mode-hook 'add-local-dev-hooks)

;; Google
(require 'google-search)
;; Set browser to Chromium
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

;; keys
(global-set-key [(control c) (c)] 'comment-or-uncomment-region)
;; (global-set-key [(control c) (v)] 'uncomment-region)
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control k)] 'kill-some-buffers)
(global-set-key [(control c) (control s)] 'google-it)
(global-set-key [(control c) (s)] 'google-search-selection)
(global-set-key [(control c) (d)] 'find-tag)
(global-set-key [(control c) (f)] 'find-tag-other-window)

;; Close the compilation window if there was no error at all.
;; (setq compilation-exit-message-function
;;       (lambda (status code msg)
;;         ;; If M-x compile exists with a 0
;;         (when (and (eq status 'exit) (zerop code))
;;           ;; then bury the *compilation* buffer, so that C-x b doesn't go there
;;           (bury-buffer "*compilation*")
;;           ;; and return to whatever were looking at before
;;           ;; (replace-buffer-in-windows "*compilation*"))
;;         ;; Always return the anticipated result of compilation-exit-message-function
;;         )))
