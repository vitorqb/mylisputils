;;; mylisputils-test.el --- Tests for mylisputils
(require 'cl-lib)

(ert-deftest add-to-generic-path-works ()
  (let ((stub-env "A"))
    (cl-letf (((symbol-function 'getenv) (lambda (x) stub-env))
              ((symbol-function 'setenv) (lambda (y x) (setq stub-env x))))
      (myutils/add-to-generic-path "B" "")
      (should (equal stub-env "B:A")))))

(ert-deftest test-add-to-python-path ()
  (let ((python-path "PATH1"))
    (cl-letf (((symbol-function 'getenv) (lambda (&rest args) python-path))
              ((symbol-function 'setenv) (lambda (y x) (setq python-path x))))
      (myutils/add-to-pythonpath "PATH2")
      (should (equal python-path "PATH2:PATH1")))))

(ert-deftest test-call-shell-command-kill-buffer ()
  (cl-letf (((symbol-function 'read-string) (-const "")))
    (myutils/call-shell-command "echo 'hola'" "mybuff")
    (should (equal (get-buffer "mybuff") nil))))

(ert-deftest test-call-shell-command-does-not-kill-buffer ()
  (cl-letf (((symbol-function 'read-string) (-const "keep")))
    (myutils/call-shell-command "echo 'hola'" "buffname")
    (should (not (equal (get-buffer "buffname") nil)))))

(ert-deftest test-concat-file ()
  (should (equal (myutils/concat-file "/home/vitor" "file") "/home/vitor/file"))
  (should (equal (myutils/concat-file "/home/vitor/" "f") "/home/vitor/f")))

(ert-deftest test-myutils/li ()
  (should (equal (macroexpand (myutils/li (message "Hola")))
                 (list (lambda () (interactive) (message "Hola"))))))

;;; mylisputils-test.el ends here
