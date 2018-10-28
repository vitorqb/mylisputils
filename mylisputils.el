(require 'dash)
(require 'dash-functional)

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

(provide 'mylisputils)
