;;; lastpass.el --- Emacs interface to LastPass      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Package-Requires: ((emacs "24.3") (csv "2.1"))
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

;; About:
;;
;; This package use `lpass' (the LastPass command line tool) to let you access
;; your LastPass within Emacs.

;; Requirements:
;;
;; (and (version<= "24.3" emacs-version) (executable-find "lpass") (require 'csv)
;;
;; - Emacs 24.3 or higher
;; - `lpass' (The LastPass command line tool)
;; - `csv.el' (can be installed with `package.el' from Melpa)

;; Usage:
;;
;; - `M-x list-lastpass'
;; - `M-x helm-lastpass'
;; - `M-x lastpass-copy-password'

;; Ideas:
;;
;; - TODO Not only read, but also add/delete/update

;; Links:
;;
;; - lpass:    https://github.com/lastpass/lastpass-cli
;; - lpass(1): https://github.com/lastpass/lastpass-cli/blob/master/lpass.1.txt


;;; Code:

(require 'cl-lib)
(require 'csv)
(require 'tabulated-list)

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
  (lastpass-login-maybe)
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


;;; User Commands

;;;###autoload
(defun lastpass-copy-password (name)
  "Copy password of NAME to kill-ring."
  (interactive
   (list
    (completing-read
     "Name: "
     (cl-loop for al in (lastpass-export)
              collect (cdr (assoc "fullname" al)))
     nil t)))
  (let ((password
         (cl-loop for al in (lastpass-export 'no)
                  for fullname = (cdr (assoc "fullname" al))
                  for password = (cdr (assoc "password" al))
                  when (string= name fullname)
                  return password)))
    (kill-new password)
    (message "Password of %s copied: %s" name password)))


;;; LastPass menu mode

(defun lastpass-menu-sort-entries (entries)
  "Sort LastPass entries like lpass ls."
  (sort entries
        (lambda (a b)
          (let ((a-group (cdr (assoc "group" a)))
                (b-group (cdr (assoc "group" b))))
            (if (string= a-group b-group)
                (let ((a-name (cdr (assoc "name" a)))
                      (b-name (cdr (assoc "name" b))))
                  (string< a-name b-name))
              (string< a-group b-group))))))

(defun lastpass-menu-mode-refresh ()
  (setq tabulated-list-entries
        (cl-loop for al in (lastpass-menu-sort-entries (lastpass-export))
                 for id = (cdr (assoc "id" al))
                 for group = (cdr (assoc "group" al))
                 for name = (cdr (assoc "name" al))
                 for username = (cdr (assoc "username" al))
                 for password = (cdr (assoc "password" al))
                 for url = (cdr (assoc "url" al))
                 for url2 = (if (string= url "http://sn")
                                ""
                              (list url
                                    'action
                                    (lambda (button)
                                      (browse-url (button-label button)))))
                 for contents = (vector name group username password url2)
                 collect (list id contents))))

(defun lastpass-menu-describe-entry ()
  "Describe the current entry."
  (interactive)
  (shell-command
   (format "%s show %s"
           (shell-quote-argument (lastpass-cli))
           (tabulated-list-get-id))))

(defvar lastpass-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'lastpass-menu-describe-entry)
    map)
  "Keymap for `lastpass-menu-mode'")

(define-derived-mode lastpass-menu-mode tabulated-list-mode "LastPass Menu"
  "Major mode for listing LastPsss entries."
  (setq tabulated-list-format [("Name" 18 t)
			       ("Group" 18)
                               ("Username" 24)
                               ("Password" 16)
                               ("URL" 0)])
  (lastpass-menu-mode-refresh)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook #'lastpass-menu-mode-refresh nil t)
  (tabulated-list-print))

;;;###autoload
(defun list-lastpass ()
  "Display a list of LastPass entries."
  (interactive)
  (let ((buffer (get-buffer-create "*LastPass List*")))
    (with-current-buffer buffer
      (lastpass-menu-mode))
    (switch-to-buffer buffer)))


;;; Helm Support

(declare-function helm "helm" (&rest plist))
(declare-function helm-make-source "helm-source" (name class &rest args))

;;;###autoload
(defun helm-lastpass ()
  "Helm interface to LastPass."
  (interactive)
  (lastpass-login-maybe)
  (unless (require 'helm)
    (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
  (helm :sources
        (helm-make-source "LastPass" 'helm-source-sync
          :candidates
          (lambda ()
            (mapcar (lambda (item)
                      (cons (cdr (assoc "fullname" item))
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
                 (message "Copied: %s" url))))))
        :buffer "*helm LastPass*"))

(provide 'lastpass)
;;; lastpass.el ends here
