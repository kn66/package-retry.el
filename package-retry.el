;;; package-retry.el --- Add retry functionality to package.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 kn66

;; Author: Nobuyuki Kamimoto
;; Version: 1.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, package, network
;; URL: https://github.com/kn66/package-retry.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds retry functionality to Emacs package.el download operations.
;; Package downloads sometimes fail due to temporary network issues
;; or server problems.  This package automatically retries failed
;; download operations only (not build/compilation) with configurable
;; delay and retry count.

;; Usage:
;;   (require 'package-retry)
;;   (package-retry-mode 1)

;; Customization:
;;   M-x customize-group RET package-retry RET

;;; Code:

(require 'package)

(defgroup package-retry nil
  "Add retry functionality to package downloads."
  :group 'package
  :prefix "package-retry-")

(defcustom package-retry-max-attempts 5
  "Maximum number of total attempts for package downloads.
This includes the initial attempt, so a value of 5 means
1 initial attempt + 4 retries.
Set to 1 to disable retry functionality (single attempt only).
Must be at least 1."
  :type '(integer :match (lambda (_widget value) (>= value 1)))
  :group 'package-retry)

(defcustom package-retry-delay 3
  "Initial delay in seconds between retry attempts.
Must be a non-negative number."
  :type '(number :match (lambda (_widget value) (>= value 0)))
  :group 'package-retry)

(defcustom package-retry-backoff-factor 2
  "Multiplier applied to the delay after each failed attempt.
A value of 1 keeps the delay fixed.  Must be at least 1."
  :type '(number :match (lambda (_widget value) (>= value 1)))
  :group 'package-retry)

(defcustom package-retry-max-delay 30
  "Maximum delay in seconds between retry attempts.
Set to nil to disable the limit."
  :type '(choice (const :tag "No limit" nil)
                 (number :tag "Seconds"
                         :match (lambda (_widget value) (>= value 0))))
  :group 'package-retry)

(defcustom package-retry-jitter 0.1
  "Maximum random variation applied to each retry delay.
The value is a fraction between 0 and 1.  For example, 0.1 varies
the calculated delay by up to 10 percent in either direction."
  :type '(number :match (lambda (_widget value)
                          (and (>= value 0) (<= value 1))))
  :group 'package-retry)

(defcustom package-retry-predicate #'package-retry--default-retry-p
  "Function deciding whether a download error should be retried.
The function receives the error data from `condition-case' and
returns non-nil when the operation may be retried."
  :type 'function
  :group 'package-retry)

(defcustom package-retry-enable-message t
  "Whether to show retry messages during package downloads."
  :type 'boolean
  :group 'package-retry)

(defvar package-retry--in-package-install nil
  "Non-nil while the compatibility advice handles a package install.")

(defconst package-retry--non-retryable-error-symbols
  '(args-out-of-range invalid-function user-error void-function
                      void-variable wrong-type-argument)
  "Error symbols that normally indicate a permanent local failure.")

(defun package-retry--default-retry-p (err)
  "Return non-nil when ERR appears suitable for another attempt.
Known programming errors and permanent HTTP or certificate failures
are rejected.  Unknown retrieval errors remain retryable."
  (let ((message-text (downcase (error-message-string err))))
    (not
     (or (memq (car err) package-retry--non-retryable-error-symbols)
         (string-match-p
          "\\(?:http\\|status\\)[^0-9]*\\(?:400\\|401\\|403\\|404\\|405\\|410\\|422\\)\\b"
          message-text)
         (string-match-p
          "certificate.*\\(?:expired\\|invalid\\|untrusted\\|hostname\\)"
          message-text)))))

(defun package-retry--retry-delay (attempt)
  "Return the delay before retrying after ATTEMPT.
Apply exponential backoff, the configured maximum, and jitter."
  (let* ((base-delay (max 0 package-retry-delay))
         (factor (max 1 package-retry-backoff-factor))
         (calculated (* base-delay (expt factor (1- attempt))))
         (capped (if package-retry-max-delay
                     (min calculated (max 0 package-retry-max-delay))
                   calculated))
         (jitter (min 1 (max 0 package-retry-jitter)))
         (random-fraction (/ (float (random 1000000)) 1000000.0))
         (variation (* capped jitter (- (* 2 random-fraction) 1))))
    (max 0 (+ capped variation))))

(defun package-retry--download-name (url args)
  "Return a human-readable download name from URL and ARGS."
  (let ((file (plist-get args :file)))
    (if file
        (concat url file)
      url)))

(defun package-retry--with-retry (thunk download-name &optional retry-p)
  "Call THUNK, retrying errors for DOWNLOAD-NAME.
When RETRY-P is non-nil, call it with the error data before
retrying.  If RETRY-P returns nil, signal the error immediately."
  (let ((max-attempts (max 1 package-retry-max-attempts))
        (attempt 1)
        result
        done)
    (while (not done)
      (condition-case err
          (setq result (funcall thunk)
                done t)
        (error
         (let ((retryable
                (and (if retry-p (funcall retry-p err) t)
                     (funcall package-retry-predicate err))))
           (cond
            ((not retryable)
             (signal (car err) (cdr err)))
            ((>= attempt max-attempts)
             (when (and package-retry-enable-message
                        (> max-attempts 1))
               (message
                "Package download failed after %d attempts: %s"
                max-attempts download-name))
             (signal (car err) (cdr err)))
            (t
             (let ((delay (package-retry--retry-delay attempt)))
               (when package-retry-enable-message
                 (message
                  "Package download failed (attempt %d/%d): %s - %s. Retrying in %.2f seconds..."
                  attempt
                  max-attempts
                  download-name
                  (error-message-string err)
                  delay))
               (sleep-for delay))
             (setq attempt (1+ attempt))))))))
    result))

(defun package-retry--with-response-buffer-retry (orig-fun url body &rest args)
  "Advice around `package--with-response-buffer-1'.
ORIG-FUN is the original function.  URL, BODY, and ARGS are its
arguments.  Only errors raised while retrieving the response are
retried; errors from BODY are signaled without retrying."
  (if (plist-get args :async)
      (apply orig-fun url body args)
    (let (body-failed)
      (package-retry--with-retry
       (lambda ()
         (setq body-failed nil)
         (apply orig-fun
                url
                (lambda ()
                  (condition-case err
                      (funcall body)
                    (error
                     (setq body-failed t)
                     (signal (car err) (cdr err)))))
                args))
       (package-retry--download-name url args)
       (lambda (_err)
         (not body-failed))))))

(defun package-retry--package-install-context (orig-fun &rest args)
  "Bind retry context around ORIG-FUN for older Emacs versions.
ARGS are passed through to ORIG-FUN."
  (let ((package-retry--in-package-install t))
    (apply orig-fun args)))

(defun package-retry--url-insert-file-contents-retry
    (orig-fun url &rest args)
  "Advice around `url-insert-file-contents' for older Emacs.
ORIG-FUN is the original function.  URL and ARGS are its
arguments."
  (if package-retry--in-package-install
      (package-retry--with-retry
       (lambda ()
         (apply orig-fun url args))
       url)
    (apply orig-fun url args)))

(defun package-retry--advice-add-once (symbol where function)
  "Add FUNCTION as advice to SYMBOL at WHERE unless already present."
  (unless (advice-member-p function symbol)
    (advice-add symbol where function)))

(defun package-retry--enable ()
  "Enable package download retry advice."
  (if (fboundp 'package--with-response-buffer-1)
      (package-retry--advice-add-once
       'package--with-response-buffer-1
       :around
       #'package-retry--with-response-buffer-retry)
    (require 'url-handlers)
    (package-retry--advice-add-once
     'package-install-from-archive
     :around
     #'package-retry--package-install-context)
    (package-retry--advice-add-once
     'url-insert-file-contents
     :around
     #'package-retry--url-insert-file-contents-retry)))

(defun package-retry--disable ()
  "Disable package download retry advice."
  (when (fboundp 'package--with-response-buffer-1)
    (advice-remove 'package--with-response-buffer-1
                   #'package-retry--with-response-buffer-retry))
  (advice-remove 'package-install-from-archive
                 #'package-retry--package-install-context)
  (when (fboundp 'url-insert-file-contents)
    (advice-remove 'url-insert-file-contents
                   #'package-retry--url-insert-file-contents-retry)))

;;;###autoload
(define-minor-mode package-retry-mode
  "Toggle package download retry functionality.
When enabled, failed package downloads will be automatically
retried according to `package-retry-max-attempts' and
`package-retry-delay' settings."
  :global t
  :group 'package-retry
  (if package-retry-mode
      (package-retry--enable)
    (package-retry--disable)))

(defun package-retry-unload-function ()
  "Unload function for package-retry.
Removes advice and disables mode when package is unloaded."
  (setq package-retry-mode nil)
  (package-retry--disable)
  ;; Return nil to allow standard unload actions
  nil)

(provide 'package-retry)

;;; package-retry.el ends here
