;;; package-retry.el --- Add retry functionality to package.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

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
  "Maximum number of retry attempts for package downloads.
Set to 1 to disable retry functionality."
  :type 'integer
  :group 'package-retry)

(defcustom package-retry-delay 3
  "Delay in seconds between retry attempts."
  :type 'number
  :group 'package-retry)

(defcustom package-retry-enable-message t
  "Whether to show retry messages during package downloads."
  :type 'boolean
  :group 'package-retry)

(defun package-retry--download-with-retry (orig-fun pkg-desc)
  "Advice function to add retry functionality to package-install-from-archive.
ORIG-FUN is the original function, PKG-DESC is the package descriptor."
  (let ((retry-count 0)
        (max-retries package-retry-max-attempts)
        (delay package-retry-delay)
        (package-name (package-desc-name pkg-desc))
        success
        result)
    (while (and (< retry-count max-retries) (not success))
      (condition-case err
          (progn
            (when (and (> retry-count 0) package-retry-enable-message)
              (message "Retrying package download (%d/%d): %s"
                       retry-count
                       max-retries
                       package-name)
              (sleep-for delay))
            (setq result (funcall orig-fun pkg-desc))
            (setq success t))
        (error
         (setq retry-count (1+ retry-count))
         (if (>= retry-count max-retries)
             (progn
               (when package-retry-enable-message
                 (message
                  "Package download failed after %d attempts: %s"
                  max-retries package-name))
               (signal (car err) (cdr err)))
           (when package-retry-enable-message
             (message
              "Package download failed (attempt %d/%d): %s - %s"
              retry-count
              max-retries
              package-name
              (error-message-string err)))))))
    result))

;;;###autoload
(define-minor-mode package-retry-mode
  "Toggle package download retry functionality.
When enabled, failed package downloads will be automatically
retried according to `package-retry-max-attempts' and
`package-retry-delay' settings."
  :global t
  :group 'package-retry
  :lighter
  " PkgRetry"
  (if package-retry-mode
      (unless (advice-member-p
               #'package-retry--download-with-retry
               'package-install-from-archive)
        (advice-add
         'package-install-from-archive
         :around #'package-retry--download-with-retry))
    (when (advice-member-p
           #'package-retry--download-with-retry
           'package-install-from-archive)
      (advice-remove
       'package-install-from-archive
       #'package-retry--download-with-retry))))

(provide 'package-retry)

;;; package-retry.el ends here
