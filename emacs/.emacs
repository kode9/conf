;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file                                                    ;;
;; Pierre-Luc Perrier <pluc@the-pluc.net>                                      ;;
;;                                                                             ;;
;; Copy this in your home                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; conf path
(add-to-list 'load-path '"~/conf/emacs/")
(add-to-list 'load-path '"~/.emacs.d/")

;; start emacs-client server
(server-start)

(setq user-full-name "")
(setq user-email "")

;; Package archive
(require 'package)
;;; Additional packages http://marmalade-repo.org/
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; zenburn color theme
(require 'zenburn)
(color-theme-zenburn)
;; (add-to-list 'custom-theme-load-path '"~/usr/src/emacs-color-theme-solarized/")
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

;; do not show the welcome message
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; shut off message buffer
(setq message-log-max nil)
;; (kill-buffer "*Messages*")

;; get rid of bells
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; frame title
(setq frame-title-format "Emacs : %f")
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Save history beetween sessions
(setq savehist-file "~/.emacs.d/history")
(savehist-mode t)

;; custom variables
(custom-set-variables
 '(tool-bar-mode nil)                   ; turn off the toolbar
 '(menu-bar-mode nil)                   ; turn off the menubar
 '(scroll-bar-mode nil)                 ; turn off the scrollbar
 '(line-number-mode t)                  ; show line number
 '(column-number-mode t)                ; show column number
 '(display-time-mode nil)               ; do not show date and time
 '(show-paren-mode t)                   ; highlight parenthesis
 '(size-indication-mode t)              ; show buffer size
 '(mouse-avoidance-mode 'cat-and-mouse) ; avoid cursor
 '(display-time-mode nil)               ; do not display time
 '(show-trailing-whitespace t)          ; trailing whitespaces
 '(python-python-command "python3")     ; use python3 by default
 '(winner-mode t)                       ; used for autoclose
 '(display-time-day-and-date)
 '(display-time-24hr-format)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; Major modes
;;; Path
(add-to-list 'load-path '"~/conf/emacs/qml-mode")
;;; Load
(autoload 'qml-mode "qml-mode" "QML mode" t)
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'cmake-mode "cmake-mode" "Cmake Mode." t)
;;;; Filename patterns
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\|mdwn\\|mdml\\)\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-mode))
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
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0")
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
(setq compilation-exit-message-function
      (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
          ;; then bury the *compilation* buffer, so that C-x b doesn't go there
          (bury-buffer "*compilation*")
          ;; and return to whatever were looking at before
          (replace-buffer-in-windows "*compilation*"))
        ;; Always return the anticipated result of compilation-exit-message-function
        )))
