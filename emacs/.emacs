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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(display-battery-mode nil)
 '(display-time-24hr-format nil)
 '(display-time-day-and-date nil)
 '(display-time-mode nil)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(mouse-avoidance-mode (quote cat-and-mouse) nil (avoid))
 '(python-python-command "python3")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(winner-mode t))

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
(package-initialize) ;; initialisation de package
;;; Additional packages http://marmalade-repo.org/
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Theme
;;; Zenburn color theme
(require 'zenburn-theme)
;;; Solarized
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)

;; Identify multiple buffers with the same file name
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ido (InteractivelyDoThings) better completion for files and buffer, fuzzy matching etc
(setq ido-everywhere t) ;; activer ido pour find-files et les buffers
(setq ido-enable-flex-matching t) ;; activer le fuzzy matching pour trier les résultats
(setq ido-use-filename-at-point 'guess)
(setq ido-save-directory-list-file "~/.emacs.d/ido.cache")
(setq ido-enable-last-directory-history t) ; remember last used dirs
(setq ido-max-work-directory-list 30)   ; should be enough
(setq ido-max-work-file-list 50)   ; remember many
(setq ido-confirm-unique-completion t) ; wait for RET, even with unique completion
(setq confirm-nonexistent-file-or-buffer nil) ;; when using ido, the confirmation is rather annoying...
(ido-mode 1) ;; activer ido

;; ido evberywhere
(ido-ubiquitous-mode t)
;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; ido for meta-x
(smex-initialize)
(global-set-key "\M-x" 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido recently opened files
(setq recentf-max-saved-items 50) ;; fixer le nombre d'enregistrements à 50
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; find-tag using ido
(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (visit-tags-table-buffer)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

;; find-tag-other-window using ido
(defun ido-find-tag-other-window ()
  "Find a tag using ido"
  (interactive)
  (visit-tags-table-buffer)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag-other-window (ido-completing-read "Tag: " tag-names))))

;; Find files in Tags File
(defun ido-find-file-in-tags ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key [(control c) (d)] 'ido-find-tag)
(global-set-key [(control c) (f)] 'ido-find-tag-other-window)
(global-set-key [(control c) (g)] 'ido-find-file-in-tags)

;; Ibuffer
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
               ("ADMS" (filename . "adms/"))
               ("DEV" (or
		       (mode . c++-mode)
		       (mode . c-mode)
		       (mode . python-mode)))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; remplace buffermenu

;; Bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks.cache")

;; windmove: move with alt + arrow
(windmove-default-keybindings 'super)

;; Show region size and maximum column number in mode-line
;;; http://www.emacswiki.org/emacs/modeline-posn.el
;;; Does not work with powerline
(set 'modelinepos-column-limit 100)

;; http://www.emacswiki.org/emacs/AutoIndentMode
(setq auto-indent-on-visit-file t)
(auto-indent-global-mode)

;; Powerline (custom mode-line)
;;; https://github.com/milkypostman/powerline

;; (defun powerline-pluc-theme ()
;;   "Setup the default mode-line."
;;   (interactive)
;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (mode-line (if active 'mode-line 'mode-line-inactive))
;;                           (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                           (separator-left (intern (format "powerline-%s-%s"
;;                                                           powerline-default-separator
;;                                                           (car powerline-default-separator-dir))))
;;                           (separator-right (intern (format "powerline-%s-%s"
;;                                                            powerline-default-separator
;;                                                            (cdr powerline-default-separator-dir))))
;;                           (lhs (list (powerline-buffer-id nil 'l)
;;                                      (when (and (boundp 'which-func-mode) which-func-mode)
;;                                        (powerline-raw which-func-format nil 'l))
;;                                      (powerline-raw " ")
;;                                      (funcall separator-left mode-line face1)
;;                                      (when (boundp 'erc-modified-channels-object)
;;                                        (powerline-raw erc-modified-channels-object face1 'l))
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-raw "%l ϟ %c" face2 'l)
;;                                      (funcall separator-left face2 face1)
;;                                      (powerline-major-mode face1 'l)
;;                                      (powerline-process face1)
;;                                      (powerline-minor-modes face1 'l)
;;                                      (powerline-narrow face1 'l)
;;                                      (powerline-raw " " face1)
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-vc face2 'r)))
;;                           (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                      (funcall separator-right face2 face1)
;;                                      (funcall separator-right face1 mode-line)
;;                                      (powerline-raw " ")
;;                                      (powerline-raw "%6p" nil 'r)
;;                                      (powerline-hud face2 face1))))
;;                      (concat (powerline-render lhs)
;;                              (powerline-fill face2 (powerline-width rhs))
;;                              (powerline-render rhs)))))))

;; (powerline-pluc-theme)
;; (powerline-default-theme)

;; https://github.com/pmarinov/clean-aindent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; View, stage and revert Git changes straight from the buffer.
;;; https://github.com/nonsequitur/git-gutter-plus
(global-git-gutter+-mode t)

;; Autoscoll compilation buffer and stop on first error
(set 'compilation-scroll-output 'first-error)
;; Skip warnings when jumping between errors
(set 'compilation-skip-threshold 2)

;; Highlights current line in compilation within another buffer
;;; Actually it's a bit annoying...
;; (add-hook 'compilation-mode-hook (lambda () (fm-start)))
;; (remove-hook 'compilation-mode-hook (lambda () (fm-start)))

;; Shorten long file-name targets
;;; https://github.com/lewang/scf-mode
;;; Seems to work in grep, but not in compile :(
(autoload 'scf-mode "scf-mode" "SCF Mode" t)
(add-hook 'compilation-mode-hook (lambda () (scf-mode t)))

;; Stop asking yes/no before compile when a compilation is already running
;;; ftp://download.tuxfamily.org/user42/compilation-always-kill.el
(autoload 'compilation-always-kill-mode "compilation-always-kill" "Compilation kill" t)
(compilation-always-kill-mode t)

;; Color FIXME/TODO/BUG/KLUDGE in comments and strings
(add-hook 'c-mode-common-hook 'turn-on-fic-mode)

;; Banner comments
(autoload 'line-comment-banner "line-comment-banner" "Comment banner" t)
(global-set-key [(control c) (b)] (lambda () (interactive) (line-comment-banner 80)))

;; Various debug function
;;; http://www.cbrunzema.de/download/ll-debug/ll-debug.el
;; (require 'll-debug)
;; ;;; Comment a region and keep a copy
;; (global-set-key [(control c) (w)] 'll-debug-copy-and-comment-region-or-line)

;; Space around operators
;; (add-to-list 'load-path '"~/.emacs.d/elpa/smart-operator-20051013.1756/")
;; (require 'smart-operator)
;; (defun smart-operator-c-hook ()
;;   (smart-insert-operator-hook)
;;   (local-unset-key (kbd "."))
;;   (local-unset-key (kbd ":"))
;;   (local-unset-key (kbd "!"))
;;   (local-unset-key (kbd "<"))
;;   (local-unset-key (kbd ">"))
;;   (local-unset-key (kbd "/"))
;;   (local-unset-key (kbd "-"))
;;   (local-set-key (kbd "*") 'c-electric-star))
;; (add-hook 'c-mode-common-hook 'smart-operator-c-hook)

;; Major modes
;;; Load
(autoload 'glsl-mode "glsl-mode" nil t)
(autoload 'qml-mode "qml-mode" "QML mode" t)
(autoload 'markdown-mode "markdown-mode" "Markdown mode" t)
(autoload 'cmake-mode "cmake-mode" "Cmake Mode." t)
(autoload 'cuda-mode "cuda-mode" "Cuda Mode." t)
(autoload 'apache-mode "apache-mode" nil t)
(autoload 'haxe-mode "haxe-mode" "Haxe Mode." t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
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
(add-to-list 'auto-mode-alist '("\\.\\(glsl\\|vert\\|frag\\|geom\\|vs\\|ksh\\|fs\\|gs\\)\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.lk\\'" . js-mode)) ; LeekScript (LeekWars)
(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

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
(yas-global-mode 1)

;;; Autocomplete
(ac-config-default)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

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
(defun dtw()
  "Delete trailing whitespaces"
  (interactive)
  (delete-trailing-whitespace))

(defun iwb()
  "Indent whole buffer, untabify, and set UTF8"
  (interactive)
  (indent-region (point-min) (point-max) nil)
  (set-buffer-file-coding-system 'utf-8-unix))

(defun dev-hooks()
  "Update copyright"
  (interactive)
  (iwb)
  (copyright-update))

(define-minor-mode pluc-mode
  "Clean buffers."
  :lighter " pluc"
  :global t
  (if pluc-mode
      (progn
        (add-hook 'before-save-hook 'dev-hooks nil t))
    (remove-hook 'before-save-hook 'dev-hooks t)))

;; Clean for any files
(add-hook 'before-save-hook 'dtw)
(add-hook 'prog-mode-hook #'pluc-mode)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; DuckDuckGo
(require 'ddg-search) ;; Search functions. Need both ddg (packaged) and ddg-mode

;;; Additional function to search the current region. Copy/paste from google-search.el
(defvar ddg-search-maxlen 50
  "Maximum string length of search term.  This prevents you from accidentally
sending a five megabyte query string to Netscape.")

(defun duckduckgo-region ()
  "Search the current region on DuckDuckGo."
  (interactive)
  (let (start end term url)
    (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
        (progn
          (setq start (region-beginning)
                end   (region-end))
          (if (> (- start end) ddg-search-maxlen)
              (setq term (buffer-substring start (+ start ddg-search-maxlen)))
            (setq term (buffer-substring start end)))
          (duckduckgo-web term))
      (beep)
      (message "Region not active"))))

(global-set-key [(control c) (control s)] 'duckduckgo-web)
(global-set-key [(control c) (s)] 'duckduckgo-region)

;; keys
(global-set-key [(control c) (c)] 'comment-or-uncomment-region)
;; (global-set-key [(control c) (v)] 'uncomment-region)
(global-set-key [(control c) (x)] 'compile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(control x) (control k)] 'kill-some-buffers)

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

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (diff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))
