;;; lastpass.el --- Emacs interface to LastPass      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Package-Requires: ((csv "2.1"))
;; Keywords: password, LastPass

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Home Page: https://github.com/lastpass/lastpass-cli
;; Man Page: https://github.com/lastpass/lastpass-cli/blob/master/lpass.1.txt

;; Some ideas:
;;
;; TODO Not just read, also change, add, delete
;; TODO Use the Org mode to show?
;; TODO Use the Tabulated List mode to show?
;; TODO Some simple Lisp interface like (lastpass-get-password "GitHub")?

;;; Code:

(require 'csv)

(defun lastpass-login-p ()
  "Check if you have logged in."
  (zerop (call-process "lpass" nil nil nil "status")))

(defun lastpass-login (&optional email password)
  "Log in using EMAIL and PASSWORD."
  (let* ((email
          (or email (read-string "Email: " user-mail-address)))
         (password
          (or password (read-passwd "Password: ")))
         (command
          (format
           "echo -n '%s' | LPASS_DISABLE_PINENTRY=1 lpass login --color=never %s"
           password email)))
    (with-temp-buffer
      (if (zerop (call-process-shell-command command nil t nil))
          (message "Success: Logged in as %s" email)
        (error "%s" (buffer-string))))))

(defun lastpass-logout ()
  "Log out."
  (with-temp-buffer
    (unless (zerop (call-process "lpass" nil t nil "logout" "--force"))
      (error "%s" (buffer-string)))))

(defun lastpass-export ()
  "Return a list of alist which contains all account information."
  (with-temp-buffer
    (if (zerop (call-process "lpass" nil t nil "export" "--color=never"))
        (csv-parse-buffer t)
      (error "%s" (buffer-string)))))

(defun helm-lastpass ()
  "Helm interface to LastPass."
  (interactive)
  (require 'helm)
  (unless (lastpass-login-p)
    (lastpass-login))
  (helm :sources
        (helm-build-sync-source "LastPass"
          :candidates
          (lambda ()
            (mapcar (lambda (item)
                      (cons (cdr (assoc "name" item))
                            item))
                    (lastpass-export)))
          :action
          '(("Visit site" .
             (lambda (candidate)
               (browse-url (cdr (assoc "url" candidate)))))
            ("Copy username" .
             (lambda (candidate)
               (let ((username (cdr (assoc "username" candidate))))
                 (unless (string= "" username)
                   (kill-new username)
                   (message "Copied: %s" username)))))
            ("Copy password" .
             (lambda (candidate)
               (let ((password (cdr (assoc "password" candidate))))
                 (unless (string= "" password)
                   (kill-new password)
                   ;; Oops, should I show it?
                   (message "Copied: %s" password)))))
            ("Copy URL" .
             (lambda (candidate)
               (let ((url (cdr (assoc "url" candidate))))
                 (kill-new url)
                 (message "Copied: %s" url))))
            ;; XXX Remove this in the future
            ("Pretty-Print (for debugging)" . #'pp)))
        :buffer "*helm LastPass*"))

(provide 'lastpass)
;;; lastpass.el ends here
