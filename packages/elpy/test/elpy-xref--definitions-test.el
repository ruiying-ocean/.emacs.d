(ert-deftest elpy-xref--definitions-should-return-definitions ()
  (when (featurep 'xref)
    (elpy-testcase ((:project project-root "test.py"))
        (find-file (f-join project-root "test.py"))
        (elpy-enable)
        (python-mode)
        (insert "def foo(x, y):\n"
                "    return x + y\n"
                "var1 = foo(5, 2)")
        (save-buffer)
        (let* ((basefile (f-join project-root "test.py"))
               (foo-defs (car (elpy-xref--definitions "1: foo")))
               (summary (xref-item-summary foo-defs))
               (location (xref-item-location foo-defs))
               (pos (xref-elpy-location-pos location))
               (file (xref-elpy-location-file location)))
          (should (string-equal summary "1:	def foo(x, y):"))
          (should (string-equal file basefile))
          (should (equal pos 5)))
        (let* ((basefile (f-join project-root "test.py"))
               (foo-defs (car (elpy-xref--definitions "3: var1")))
               (summary (xref-item-summary foo-defs))
               (location (xref-item-location foo-defs))
               (pos (xref-elpy-location-pos location))
               (file (xref-elpy-location-file location)))
          (should (string-equal summary "3:	var1 = foo(5, 2)"))
          (should (string-equal file basefile))
          (should (equal pos 33))))))
