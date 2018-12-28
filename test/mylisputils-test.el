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
  (should (equal (myutils/li (message "Hola"))
                 '(lambda () (interactive) (message "Hola")))))

(ert-deftest test-myutils//clean-buffers-should-kill-p ()
  (-let [myutils/clean-buffers-names-regexs (list "hola")]
    (-let [buff (generate-new-buffer "hola")]
      (should (myutils//clean-buffers-should-kill-p buff)))
    (-let [buff (generate-new-buffer "rola")]
      (should (not (myutils//clean-buffers-should-kill-p buff))))))

(ert-deftest test-myutils//clean-buffers ()
  (-let [myutils/clean-buffers-names-regexs (list "AA")]
    (-let [buff (generate-new-buffer "AA")]
      (myutils/clean-buffers)
      (should (not (buffer-live-p buff))))
    (-let [buff (generate-new-buffer "BB")]
      (myutils/clean-buffers)
      (should (buffer-live-p buff)))))

(ert-deftest test-myutils/fill-to-end ()
  ;; Single usage
  (with-temp-buffer
    (myutils/fill-to-end)
    (should (equal (point) 1))
    (should (equal (buffer-string) (make-string 80 ?-))))
  ;; Line with previous content
  (with-temp-buffer
    (insert (make-string 40 ?A))
    (myutils/fill-to-end)
    (should (equal (buffer-string)
                   (concat (make-string 40 ?A) (make-string 40 ?-)))))
  ;; Usage when col after 80
  (with-temp-buffer
    (-let [inserted-text (make-string 80 ?B)]
      (insert inserted-text)
      (should (equal (buffer-string) inserted-text))
      (myutils/fill-to-end)
      (should (equal (buffer-string) inserted-text)))))

(ert-deftest test-myutils/remove-whitespace-and-newline/base ()
    (with-temp-buffer
      (insert "B   \n \t  C")
      (goto-char 2)
      (should (equal (string (char-after)) " "))
      (myutils/remove-whitespace-and-newline)
      (should (equal (char-after) ?C))))

(ert-deftest test-myutils/remove-whitespace-and-newline/delete-none ()
  (with-temp-buffer
    (-let [text "ABC"]
      (insert text)
      (myutils/remove-whitespace-and-newline)
      (should (equal (buffer-string) text)))))

(ert-deftest test-myutils/remove-whitespace-and-newline/end-of-buffer ()
  (with-temp-buffer
    (-let [text "ABC"]
      (insert text)
      (end-of-buffer)
      (myutils/remove-whitespace-and-newline)
      (should (equal (buffer-string) text)))))
;;; mylisputils-test.el ends here
