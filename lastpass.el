;;; lastpass.el --- Emacs interface to LastPass      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Package-Requires: ((csv "2.1") (emacs "24.1"))
;; Keywords: password, LastPass
;; Version: 0.1

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

;; This package use lpass (the LastPass command line tool) to let you access
;; your LastPass within Emacs.

;; Usage:
;;
;; - If you have Helm installed, you can run the Emacs command `helm-lastpass'.

;; Notes to myself:
;;
;; Userful links:
;; lpass:    https://github.com/lastpass/lastpass-cli
;; lpass(1): https://github.com/lastpass/lastpass-cli/blob/master/lpass.1.txt
;;
;; Some ideas:
;; TODO Use the Tabulated List mode to show the result
;; TODO Use the Org mode to show the result
;; TODO Not only read, but also add/delete/update

;;; Code:

(require 'cl-lib)
(require 'csv)

(defgroup lastpass nil
  "LastPass interface for Emacs."
  :group 'tools)

(defcustom lastpass-cli "lpass"
  "The program name of the LastPass command line too."
  :type 'string
  :group 'lastpass)

(defun lastpass-cli ()
  (executable-find lastpass-cli))

(defun lastpass-login-p ()
  "Return t if has logined."
  (zerop (call-process (lastpass-cli) nil nil nil "status")))

(defun lastpass-login (&optional email password)
  "Log in with EMAIL and PASSWORD."
  (let* ((email
          (or email (read-string "Email: " user-mail-address)))
         (password
          (or password (read-passwd "Password: ")))
         (command
          ;; XXX Is there any better solution?
          (format "echo -n '%s' | LPASS_DISABLE_PINENTRY=1 %s login --color=never %s"
                  password
                  (shell-quote-argument (lastpass-cli))
                  email)))
    (with-temp-buffer
      (if (zerop (call-process-shell-command command nil t nil))
          (message "Success: Logged in as %s" email)
        (error "%s" (buffer-string))))))

(defun lastpass-login-maybe ()
  "Login if not already."
  (unless (lastpass-login-p)
    (lastpass-login)))

(defun lastpass-logout ()
  "Log out."
  (with-temp-buffer
    (unless (zerop (call-process (lastpass-cli) nil t nil "logout" "--force"))
      (error "%s" (buffer-string)))))

(defun lastpass-export (&optional sync)
  "Return a list of alist which contains all account information."
  (let ((sync (pcase sync
                ('nil   "--sync=auto")
                ('auto "--sync=auto")
                ('now  "--sync=now")
                ('no   "--sync=no")
                (_     (error "invalid argument '%s'" sync))))
        (fields (concat
                 "--fields="
                 (mapconcat #'identity
                            '("id"
                              "url"
                              "username"
                              "password"
                              "extra"
                              "name"
                              "fav"
                              "id"
                              "grouping"
                              "group"
                              "fullname"
                              "last_touch"
                              "last_modified_gmt"
                              "attachpresent")
                            ","))))
    (with-temp-buffer
      (if (zerop (call-process (lastpass-cli) nil t nil "export" "--color=never" sync fields))
          (csv-parse-buffer t)
        (error "%s" (buffer-string))))))

(defun lastpass-search (name)
  (cl-loop for al in (lastpass-export)
           when (string-match-p name (cdr (assoc "fullname" al)))
           collect al))


;;; Helm Support

(declare-function helm "helm")
(declare-function helm-build-sync-source "helm-source")

;;;###autoload
(defun helm-lastpass ()
  "Helm interface to LastPass."
  (interactive)
  (lastpass-login-maybe)
  (unless (require 'helm)
    (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
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
