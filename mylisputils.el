;;; mylisputils.el --- Vitor's utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2018-10-28
;; Keywords: elisp
;; Homepage: https://bitbucket.org/vitorqb/mylisputils/

;; This file is not part of GNU Emacs.
     
;; Do whatever you want. No warranties.

;;; code
(require 'dash)
(require 'dash-functional)
(require 'mycompile)

;; Variables that should be kept dynamic
(defvar python-shell--interpreter)
(defvar python-shell--interpreter-args)

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

;; -----------------------------------------------------------------------------
;; Python utils
;; -----------------------------------------------------------------------------
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

(defun myutils/run-django-management-command (manage-path buff-name)
  "For django. Prompts the user for a manage.py command and run it.
manage-path must be the entire path to manage.py."
  (interactive)
  (let* ((manage-cmd-opts '("showmigrations" "makemigrations" "migrate"))
         (manage-cmd (completing-read "Choose a command: " manage-cmd-opts))
         (manage-cmd-args (read-string "With args: "))
         (cmd (list "python" manage-path manage-cmd manage-cmd-args)))
    (mycompile (string-join cmd " ") buff-name t)))

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
