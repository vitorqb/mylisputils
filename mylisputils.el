;;; mylisputils.el --- Vitor's utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2018-10-28
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/mylisputils/blob/development/mylisputils.el

;; This file is not part of GNU Emacs.
     
;; Do whatever you want. No warranties.

;;; code
(require 'dash)
(require 'dash-functional)
(require 's)

;; Variables that should be kept dynamic
(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

;; Customizable variables
(defvar myutils/clean-buffers-names-regexs
  '("\\*ag search.+" "\\*Occur\\*" "magit-\\(log\\|diff\\)")
  "A list of regexp for buffers to kill when cleaning, if name matches.")

(defvar myutils/frozen-files-dir
  "~/frozen-files"
  "A directory used by the `freeze-file` command to freeze a file.")

(defvar myutils/known-datetime-formats
  '("%Y-%m-%dT%H:%M:%S"
    "%Y-%m-%d"
    "%Y%m%d"
    "%Y%m%dT%H%M%S")
  "A list of known date(time) formats.")

;; Functions
(defun myutils/add-to-generic-path (x y)
  "Adds 'x' to some environmental variable 'y' (like PYTHONPATH)"
  (setenv y (concat x ":" (getenv y))))

(defun myutils/add-to-path (x)
  "Adds the string x to the env var PATH"
  (myutils/add-to-generic-path x "PATH"))

(defun myutils/add-to-pythonpath (x)
  "Adds the string x to the environmental variable PYTHONPATH"
  (myutils/add-to-generic-path x "PYTHONPATH"))

(defun myutils/add-to-mypypath (x)
  "Adds the string x to the environmental variable MYPYPATH"
  (myutils/add-to-generic-path x "MYPYPATH"))

(cl-defun myutils/already-in-path? (x &optional (path-var "PATH"))
  "Returns t if `x` in the environmental PATH-like variable `path-var`.
  `path-var` defaults to \"PATH\"."
  (-let [equal-to-x? (-partial 'string-equal x)]
    (->> path-var getenv (s-split ":") (-any? equal-to-x?))))

(defun myutils/relative-path (destination &optional origin)
  "Uses `realpath` to find the path for `destination` relative to `origin`"
  (let* ((origin (or origin default-directory))
         (quoted-origin (shell-quote-argument origin))
         (quoted-destination (shell-quote-argument destination)))
    (-> "realpath --relative-to=%s %s"
        (format quoted-origin quoted-destination)
        (shell-command-to-string)
        (s-trim))))

(defun myutils/copy-relative-path (destination)
  "Same as `myutils/relative-path`, but don't take origin and copies to kill ring"
  (interactive "fFile:")
  (kill-new (myutils/relative-path destination)))

(defun myutils/call-shell-command (c &optional bname)
  "Calls a shell command, put's the result in a new buffer
  and prompts to user whether to keep it or not.
  c -> the shell command 
  bname -> name of the buffer where to put it (may change if exists)"
  (interactive (list (read-shell-command "Shell command: "
                                         (car shell-command-history))))
  (let ((prompt "'keep' to keep the buffer or RET to kill it: ")
	(buff (generate-new-buffer (or bname "*MyShellCommand*"))))
    (display-buffer buff)
    (async-shell-command c buff buff)
    ;; Remove sentinels so we are not told when process finishes
    (set-process-sentinel (get-buffer-process buff) #'ignore)
    (if (-> prompt (read-string) (equal "keep") (not))
	(kill-buffer buff))))

(defun myutils/concat-file (dir file)
  "Concat a file in a dir"
  (concat (file-name-as-directory dir) file))

(defmacro myutils/li (body)
  "Expands to an interactive lambda with no arguments"
  `(lambda () (interactive) ,body))

(defun myutils/fill-to-end ()
  "Fills the screen with '-' until column 80"
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char ?-))))

(defun myutils/insert-date ()
  "Inserts the curent date as YYYYMMDD"
  (interactive)
  (insert (format-time-string "%Y%m%d")))

(defun myutils/date-in-all-formats ()
  "Returns a list of all date formats."
  (--map (format-time-string it) myutils/known-datetime-formats))

(defun myutils/insert-formated-date ()
  "Shows the user different formats for the date and asks it to choose one to insert or copy."
  (interactive)
  (ivy-read
   "Date(time): "
   (myutils/date-in-all-formats)
   :action '(1
             ("i" insert "Insert")
             ("w" kill-new "Copy"))))

(defun myutils/remove-whitespace-and-newline ()
  "Removes next character until it is no longer whitespace or newline"
  (interactive)
  (-let [chars-to-delete '(?\n ?\s ?\t)]
    (while (-as-> (following-char) it
                  (-partial #'char-equal it)
                  (-any? it chars-to-delete))
      (delete-char 1))))

;; clean-buffers
(defun myutils/clean-buffers ()
  "Clean buffers whose names matches myutils/clean-buffers-names-regexs"
  (interactive)
  (--> (buffer-list)
       (-filter #'myutils//clean-buffers-should-kill-p it)
       (-each it #'kill-buffer)))

(defun myutils//clean-buffers-should-kill-p (buff)
  "Should buff be killed based on its name?"
  (--> buff
       (buffer-name it)
       (-rpartial 'string-match-p it)
       (-some? it myutils/clean-buffers-names-regexs)))

(defun myutils/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (->> (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))
       (kill-new)
       (message "Copied %s to the clipboard!")))

(defun myutils/copy-file-path-from-other-window-to-clipboard ()
  "Calls `other-window`, `copy-file-path-to-clipboard`, and comes back"
  (interactive)
  (save-window-excursion
    (call-interactively #'other-window)
    (myutils/copy-file-path-to-clipboard)))

(defun myutils/duplicate-buffer ()
  "Displays a copy of the current buffer in a new buffer and switch to it"
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(defun myutils/chmod-current-buffer (mode)
  "Like chmod but with current buffer"
  (interactive (list (read-file-modes "New mode: " (buffer-file-name))))
  (chmod (buffer-file-name) mode))

(defun myutils/message-if-display-is-empty (msg)
  "Sends the user a message if current-message returns null"
  (if (equal (current-message) nil)
      (message msg)))

(defmacro myutils/with-compile-opts (buffname cmd &rest body)
  "Evaluates body after binding compilation functions to set the
  buffer name and the default command"
  (declare (indent 2))
  `(-let ((compilation-buffer-name-function (-const ,buffname))
          (compile-command ,cmd))
     ,@body))

(defun myutils/truncate-compile-buffer (&optional n buff)
  "Truncates the max rows of the compile buffer."
  (with-current-buffer (or buff (current-buffer))
    (set (make-local-variable 'comint-buffer-maximum-size) (or n 2500))
    (add-hook 'compilation-filter-hook 'comint-truncate-buffer nil t)))

(defun myutils/compile-with-tramp-sudo (default-dir command)
  (interactive)
  (-let ((default-directory (concat "/sudo::" default-dir))
         (compile-command command))
    (-let [compilation-buffer (call-interactively #'compile)]
      (switch-to-buffer-other-window compilation-buffer))))

(defun myutils/remove-with-elipsis ()
  "Removes the current line and inserts an elipsis [...]
   If an elipsis is on the current line, removes the previous one."
  (interactive)
  (cl-flet ((current-line-is-elipsis
             ()
             (string-match-p "^\\[\\.\\.\\.\\]$" (thing-at-point 'line t))))
    (if (current-line-is-elipsis)
        ;; Already have an elipsis, kill prev line
        (progn
          (beginning-of-line 0)
          (kill-whole-line))
      ;; No elipsis, put one
      (progn
        (kill-whole-line)
        (save-excursion (insert "[...]\n"))))))

(defun myutils/freeze-file (file-name)
  (interactive (list (buffer-file-name)))
  (when (or (not file-name) (not (file-readable-p file-name)))
    (error "Could not find file to freeze."))
  (-let* ((file-name-no-dir (file-name-nondirectory file-name))
          (datetime-str (format-time-string "%Y-%m-%dT%H:%M:%S"))
          (frozen-file-name (format "%s_%s" file-name-no-dir datetime-str))
          (destination (myutils/concat-file myutils/frozen-files-dir
                                            frozen-file-name)))
    (copy-file file-name destination)
    (message (format "Copied %s to %s" file-name destination))))

(defun myutils/copy-buffer-contents ()
  (interactive)
  (copy-region-as-kill (point-min) (point-max)))

;; -----------------------------------------------------------------------------
;; Python utils
;; -----------------------------------------------------------------------------
(defvar myutils/isort-cmd "isort -y"
  "Command used to call isort")

(defun myutils/drop-to-python-shell (buff)
  "Drops the buffer to a python shell (for example if you are in a test
buffer and a (PDB) appears)."
  (interactive (list (current-buffer)))
  (with-current-buffer buff
    (setq buffer-read-only nil)
    (let ((python-shell--interpreter nil)
          (python-shell--interpreter-args nil)
          (buff-process (get-buffer-process buff)))
      (set-process-filter buff-process 'comint-output-filter)
      (inferior-python-mode)
      (yas-minor-mode-on))))

(defun myutils/compilation-filter-drop-to-python-on-pdb (buff-getter)
  "When compilation enters '(Pdb)', activates the inferior-python-mode
on the buffer returned by buff-getter"
  (let ((line (buffer-substring compilation-filter-start (point))))
    (cl-flet ((string-has-pdb? (x) (string-match-p (regexp-quote "(Pdb)") x)))
      (when (string-has-pdb? line)
        (myutils/drop-to-python-shell (funcall buff-getter))))))

(defun myutils/search-back-last-non-lib-error ()
  "Int a pytest result, uses re-search-backward to find the last error
that does not have /lib in it's path"
  (interactive)
  ;; Matches lines not beggining with space that have the pattern
  ;; ...blablabla.py:<line_number>:
  (re-search-backward "^\\([^\s\n\t]\\)\\(.+\\)\.py:[0-9]+:")
  ;; When the found line has a /lib/, call itself again.
  (when (->> (thing-at-point 'line) (string-match "^.+/lib/.+$"))
    (myutils/search-back-last-non-lib-error)))

(defun myutils/call-isort-on-current-file ()
  "Calls isort on the current file"
  (interactive)
  (when (buffer-modified-p)
    (error "Can not be called with buffer that has been modified."))
  (shell-command (concat myutils/isort-cmd " " (buffer-file-name)))
  (revert-buffer :ignore-auto :noconfirm))

(defun myutils/run-django-management-command (manage-path)
  "For django. Prompts the user for a manage.py command and run it.
manage-path must be the entire path to manage.py."
  (interactive)
  (let* ((manage-cmd-opts '("showmigrations" "makemigrations" "migrate"))
         (manage-cmd (completing-read "Choose a command: " manage-cmd-opts))
         (manage-cmd-args (read-string "With args: "))
         (cmd (list "python" manage-path manage-cmd manage-cmd-args)))
    (compile (string-join cmd " ") t)))

(defun myutils/python-django-def-class-field-occur ()
  "Calls occur for common python and django definitions of functions,
classes and fields."
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(def\\s-\\|class\\s-\\|=.+\\(Field\\|Key\\)\\)")))

(defvar myutils/prompt-for-python-invoke--prompt-fun
  (lambda (opts) (completing-read "Choose a task: " opts))
  "Function used to prompt users for a python invoke command.")

(defvar myutils/prompt-for-python-invoke--get-tasks-list-fun
  (lambda () (shell-command-to-string "inv --list"))
  "Function used to get the list of tasks for invoke.")

(defun myutils/prompt-for-python-invoke ()
  "Reads all invoke tasks calling inv --list, and prompts the user to select one"
  (interactive)
  (->> (funcall myutils/prompt-for-python-invoke--get-tasks-list-fun)
       (s-lines)                     ;Split into lines
       (cdr)                         ;Remove title
       (-map #'s-trim)               ;Trims
       (-map (-partial #'s-match "^[^ ]+")) ;Takes the first word of each line
       (-filter #'identity)          ;Remove non matches
       (-map #'car)                  ;Gets the first match
       (funcall myutils/prompt-for-python-invoke--prompt-fun)))

(defun myutils/python-invoke-task ()
  "Prompts the user for an invoke task and calls invoke"
  (interactive)
  (compile (concat "invoke " (myutils/prompt-for-python-invoke)) t))


(defun myutils/python-activate-venv ()
  "Activates a virtual environment."
  (interactive)
  (-if-let (venv-dir (myutils/python-get-default-venv-path))
      (->> venv-dir
           ((lambda (x) (pyvenv-activate x) x))
           (format "Activated venv %s!")
           (myutils/message-if-display-is-empty))
    (call-interactively #'pyvenv-activate)))

(defun myutils/python-get-default-venv-path ()
  "Looks for a venv folder in the current dir. Either returns a string with the
   path for it or nil."
  (-some->> '("venv" ".venv")
            (-map (-partial #'myutils/concat-file default-directory))
            (-filter #'file-directory-p)
            (car)))

;;------------------------------------------------------------------------------
;; Js Utils
;; -----------------------------------------------------------------------------
(defun myutils/flycheck-eslint/set-executable ()
  "Prompts the user for a eslint executable."
  (interactive)
  (let* ((project-root (read-directory-name
                        "Project root (where node_modules is): "))
         (path (myutils/concat-file project-root "node_modules/.bin/eslint")))
    (when (not (file-exists-p path))
      (error (format "ERROR -> File %s does not exist" path)))
    (setq flycheck-javascript-eslint-executable path)
    (message "flycheck-javascript-eslint-executable set to %s" path)))

(defun myutils/jest/occur-with-tests ()
  "Runs occur to find the definition of tests"
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "\\(it\\|describe\\|test\\)(.+)")))

(defun myutils/set-eslint-from-node-modules ()
  "Looks for a `node_modules` if the directory hierarchy and, if finds,
set's flycheck-javascript-eslint-executable to use the eslint from the
node_modules instalation."
  (interactive)
  (-some--> (or (buffer-file-name) default-directory)
            (locate-dominating-file it "node_modules")
            (myutils/concat-file it "node_modules/.bin/eslint")
            (and (file-executable-p it) it)
            (setq flycheck-javascript-eslint-executable it)
            (message (format "Set flycheck-javascript-eslint-executable to %s" it))))

(defun myutils/active-flycheck-for-js ()
  "Prepares js2-mode to use eslint from local node_modules"
  (interactive)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (add-hook 'js2-mode-hook #'myutils/set-eslint-from-node-modules)
  (add-hook 'js2-mode-hook 'flycheck-mode-on-safe))

;; -----------------------------------------------------------------------------
;; Cider/Clojure
;; -----------------------------------------------------------------------------
(defvar myutils/cider-default-port 4123)
(defvar myutils/cider-default-host "127.0.0.1")

(defun myutils/cider-quick-connect ()
  "Calls cider-connect with default port and host"
  (interactive)
  (cider-connect (list :host myutils/cider-default-host
                       :port myutils/cider-default-port)))

(defun myutils/clojure-occur-def ()
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^[ ]*(def+")))

;;
;;
;; 

(provide 'mylisputils)
;;; mylisputils.el ends here
