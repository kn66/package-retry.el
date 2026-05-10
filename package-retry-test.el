;;; package-retry-test.el --- Tests for package-retry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 kn66

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'package-retry)

(defmacro package-retry-test--with-fast-retry (&rest body)
  "Run BODY with retry settings suitable for testing."
  `(let ((package-retry-max-attempts 3)
         (package-retry-delay 0)
         (package-retry-enable-message nil))
     ,@body))

(ert-deftest package-retry--with-retry-eventually-succeeds ()
  (package-retry-test--with-fast-retry
   (let ((calls 0))
     (should
      (eq (package-retry--with-retry
           (lambda ()
             (setq calls (1+ calls))
             (if (< calls 3)
                 (error "Temporary failure")
               'ok))
           "test-download")
          'ok))
     (should (= calls 3)))))

(ert-deftest package-retry--with-response-buffer-retries-retrieval-errors ()
  (package-retry-test--with-fast-retry
   (let ((calls 0)
         (body-calls 0))
     (should
      (eq (package-retry--with-response-buffer-retry
           (lambda (_url body &rest _args)
             (setq calls (1+ calls))
             (if (< calls 3)
                 (error "Temporary failure")
               (funcall body)))
           "https://example.invalid/packages/"
           (lambda ()
             (setq body-calls (1+ body-calls))
             'ok)
           :file "pkg-1.0.el")
          'ok))
     (should (= calls 3))
     (should (= body-calls 1)))))

(ert-deftest package-retry--with-response-buffer-does-not-retry-body-errors ()
  (package-retry-test--with-fast-retry
   (let ((calls 0))
     (should-error
      (package-retry--with-response-buffer-retry
       (lambda (_url body &rest _args)
         (setq calls (1+ calls))
         (funcall body))
       "https://example.invalid/packages/"
       (lambda ()
         (error "Unpack failure"))
       :file "pkg-1.0.el"))
     (should (= calls 1)))))

(ert-deftest package-retry--with-response-buffer-does-not-retry-async ()
  (package-retry-test--with-fast-retry
   (let ((calls 0))
     (should-error
      (package-retry--with-response-buffer-retry
       (lambda (&rest _args)
         (setq calls (1+ calls))
         (error "Async setup failure"))
       "https://example.invalid/packages/"
       #'ignore
       :async t))
     (should (= calls 1)))))

(ert-deftest package-retry--url-insert-retries-only-in-install-context ()
  (package-retry-test--with-fast-retry
   (let ((calls 0)
         (package-retry--in-package-install nil))
     (should-error
      (package-retry--url-insert-file-contents-retry
       (lambda (&rest _args)
         (setq calls (1+ calls))
         (error "Outside package install"))
       "https://example.invalid/pkg.el"))
     (should (= calls 1))))
  (package-retry-test--with-fast-retry
   (let ((calls 0)
         (package-retry--in-package-install t))
     (should
      (eq (package-retry--url-insert-file-contents-retry
           (lambda (&rest _args)
             (setq calls (1+ calls))
             (if (< calls 2)
                 (error "Temporary failure")
               'ok))
          "https://example.invalid/pkg.el")
          'ok))
     (should (= calls 2)))))

(ert-deftest package-retry--advice-add-once-is-idempotent ()
  (let ((symbol (make-symbol "package-retry-test--advice-target"))
        (calls 0))
    (unwind-protect
        (let ((advice (lambda (orig-fun &rest args)
                        (setq calls (1+ calls))
                        (apply orig-fun args))))
          (fset symbol (lambda () 'ok))
          (package-retry--advice-add-once symbol :around advice)
          (package-retry--advice-add-once symbol :around advice)
          (should (eq (funcall symbol) 'ok))
          (should (= calls 1)))
      (when (fboundp symbol)
        (fmakunbound symbol)))))

(provide 'package-retry-test)

;;; package-retry-test.el ends here
