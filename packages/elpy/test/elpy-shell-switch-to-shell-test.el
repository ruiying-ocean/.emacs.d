(ert-deftest elpy-shell-switch-to-shell-should-switch-to-shell-buffer ()
  (elpy-testcase ()
    (elpy-enable)
    (python-mode)
    (elpy-shell-switch-to-shell)
    (should (eq major-mode 'inferior-python-mode))))