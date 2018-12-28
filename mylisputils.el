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
(require 'mycompile)

;; Variables that should be kept dynamic
(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

;; Customizable variables
(defvar myutils/clean-buffers-names-regexs
  '("\\*ag search.+" "\\*Occur\\*" "magit-\\(log\\|diff\\)")
  "A list of regexp for buffers to kill when cleaning, if name matches.")

(defun myutils/add-to-generic-path (x y)
  "Adds 'x' to some environmental variable 'y' (like PYTHONPATH)"
  (setenv y (concat x ":" (getenv y))))

(defun myutils/add-to-pythonpath (x)
  "Adds the string x to the environmental variable PYTHONPATH"
  (myutils/add-to-generic-path x "PYTHONPATH"))

(defun myutils/add-to-mypypath (x)
  "Adds the string x to the environmental variable MYPYPATH"
  (myutils/add-to-generic-path x "MYPYPATH"))

(defun myutils/call-shell-command (c bname)
  "Calls a shell command, put's the result in a new buffer
  and prompts to user whether to keep it or not.
  c -> the shell command 
  bname -> name of the buffer where to put it (may change if exists)"
  (interactive "P")
  (let ((prompt "'keep' to keep the buffer or RET to kill it: ")
	(buff (generate-new-buffer bname)))
    (display-buffer buff)
    (shell-command c buff buff)
    (if (not (equal (read-string prompt) "keep"))
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
  (->> (buffer-list)
       (-filter #'myutils//clean-buffers-should-kill-p)
       (-map #'kill-buffer)))

(defun myutils//clean-buffers-should-kill-p (buff)
  "Should buff be killed based on its name?"
  (-let [buffnm (buffer-name buff)]
    (-any? (lambda (r) (string-match-p r buffnm))
           myutils/clean-buffers-names-regexs)))

;; -----------------------------------------------------------------------------
;; Python utils
;; -----------------------------------------------------------------------------
(defvar myutils/isort-cmd "isort -y"
  "Command used to call isort")

(defun myutils/drop-to-python-shell (buff)
  "Drops the buffer to a python shell (for example if you are in a test
buffer and a (PDB) appears)."
  (with-current-buffer buff
    (setq buffer-read-only nil)
    (let ((python-shell--interpreter nil)
          (python-shell--interpreter-args nil)
          (buff-process (get-buffer-process buff)))
      (set-process-filter buff-process 'comint-output-filter)
      (inferior-python-mode))))

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
  (shell-command myutils/isort-cmd))

(defun myutils/run-django-management-command (manage-path buff-name)
  "For django. Prompts the user for a manage.py command and run it.
manage-path must be the entire path to manage.py."
  (interactive)
  (let* ((manage-cmd-opts '("showmigrations" "makemigrations" "migrate"))
         (manage-cmd (completing-read "Choose a command: " manage-cmd-opts))
         (manage-cmd-args (read-string "With args: "))
         (cmd (list "python" manage-path manage-cmd manage-cmd-args)))
    (mycompile (string-join cmd " ") buff-name t)))

(defun myutils/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun myutils/duplicate-buffer ()
  "Displays a copy of the current buffer in a new buffer and switch to it"
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))


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


(provide 'mylisputils)
;;; mylisputils.el ends here


