;;; package-retry.el --- Add retry functionality to package.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 kn66

;; Author: Nobuyuki Kamimoto
;; Version: 1.0.0
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
;; or server problems. This package automatically retries failed
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
  "Delay in seconds between retry attempts.
Must be a non-negative number."
  :type '(number :match (lambda (_widget value) (>= value 0)))
  :group 'package-retry)

(defcustom package-retry-enable-message t
  "Whether to show retry messages during package downloads."
  :type 'boolean
  :group 'package-retry)

(defun package-retry--download-with-retry (orig-fun pkg-desc)
  "Advice function to add retry functionality to package-install-from-archive.
ORIG-FUN is the original function, PKG-DESC is the package descriptor."
  (let* ((max-attempts (max 1 package-retry-max-attempts))
         (delay (max 0 package-retry-delay))
         (package-name (package-desc-name pkg-desc))
         (current-attempt 1)
         success
         result)
    (while (and (<= current-attempt max-attempts) (not success))
      (condition-case err
          (progn
            (setq result (funcall orig-fun pkg-desc))
            (setq success t))
        (error
         (if (>= current-attempt max-attempts)
             (progn
               (when package-retry-enable-message
                 (message
                  "Package download failed after %d attempts: %s"
                  max-attempts package-name))
               (signal (car err) (cdr err)))
           (when package-retry-enable-message
             (message
              "Package download failed (attempt %d/%d): %s - %s. Retrying in %d seconds..."
              current-attempt
              max-attempts
              package-name
              (error-message-string err)
              delay))
           (sleep-for delay)
           (setq current-attempt (1+ current-attempt))))))
    result))

;;;###autoload
(define-minor-mode package-retry-mode
  "Toggle package download retry functionality.
When enabled, failed package downloads will be automatically
retried according to `package-retry-max-attempts' and
`package-retry-delay' settings."
  :global t
  :group 'package-retry
  (if package-retry-mode
      (advice-add 'package-install-from-archive
                  :around #'package-retry--download-with-retry)
    (advice-remove 'package-install-from-archive
                   #'package-retry--download-with-retry)))

(defun package-retry-unload-function ()
  "Unload function for package-retry.
Removes advice and disables mode when package is unloaded."
  (when package-retry-mode
    (package-retry-mode -1))
  ;; Return nil to allow standard unload actions
  nil)

(provide 'package-retry)

;;; package-retry.el ends here
