;;; hide-secrets.el --- A package for hiding secrets in buffers  -*- lexical-binding: t; -*-
;; Copyright © 2020, by Sebastian Meisel

;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;; Version: 0.5
;; Created:  May 5, 2024
;; Keywords: tools
;; Homepage: https://gitlab.com/ostseepinguin1/hide-secrets-el
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides functions for hiding secrets like IP addresses,
;; hash sums and passwords in buffers.

;; Put (require 'hide-secrets) in you Emacs init file.

;;; Code:


;; variables
(when (string< emacs-version "30.1")
  (defconst password-colon-equivalents
    '(?\u003a ; ?\N{COLON}
      ?\uff1a ; ?\N{FULLWIDTH COLON}
      ?\ufe55 ; ?\N{SMALL COLON}
      ?\ufe13 ; ?\N{PRESENTATION FORM FOR VERTICAL COLON}
      ?\u17d6 ; ?\N{KHMER SIGN CAMNUC PII KUUH}
      )))

(defconst hide-secrets-name-word-equivalents
  '("name" "имя" "नाम" "nombre" "nom" "nome" "নাম" "naam" "नाव" "പേര്" "பெயர்"
    "పేరు" "ਨਾਮ" "नाम" "নাম" "jina" "nom" "නම" "نام" "اسم" "שם" "navn" "nom"
    "nombre" "nafn" "نام" "nimi" "nome" "imya" "பெயர்" "الاسم" "ім'я" "اسم" "名"))

(defconst hide-secrets-user-word-equivalents
  '("user" "Benutzer" "пользователь" "उपयोगकर्ता" "usuario" "utilisateur" "utente"
    "ব্যবহারকারী" "gebruiker" "वापरकर्ता" "ഉപയോക്താവ്" "பயனர்" "వాడుకరి" "ਯੂਜ਼ਰ"
    "प्रयोगकर्ता" "प्रयोगकर्ता" "ବ୍ୟବହାରକର୍ତ୍ତା" "mtumiaji" "notandi" "භාෂාවෙහි පරිශීලක" "صارف"
    "משתמש" "bruger" "bruker" "notandi" "käyttäjä" "kasutaja" "utilizador"
    "mtumiaji" "користувач" "用戶" "用户" "yonghu"))

(defvar hide-secrets-current-secret
  nil
  "Save current secret type when revealed with reveal mode.")

;; customization

(defgroup hide-secrets nil
  "Hide various secrets in Emacs buffers."
  :group 'convenience)

(defcustom hide-secrets-alist
  '(("username" . (:regexp (concat
			    (rx-to-string
			     `(: bol (* nonl)
      				 (or
				  (| . ,hide-secrets-user-word-equivalents)
				  (| . ,hide-secrets-name-word-equivalents)
				  (seq
				   (| . ,hide-secrets-user-word-equivalents)
				   (zero-or-one blank)
				   (| . ,hide-secrets-name-word-equivalents)))
				 (* nonl) (any . ,password-colon-equivalents)
				 (? "\^@") (* blank)))
			    "\\(.*\\)")
			   :match 1
			   :display "Max Mustermann"))
    ("phone" . (:regexp (rx (seq
			     (or (not "0") (not "+"))
			     (group
			      (seq
			       (or "+" "0")
			       (repeat 2 2 digit)
			       (zero-or-more (or digit " " "-"))
			       (one-or-more digit)))))
			:match 0
			:display "0123 456 789"))
    ("email" . (:regexp (rx (one-or-more (or alnum "." "_" "-" "+"))
			    "@"
			    (one-or-more (or alnum "." "_" "-"))
			    "." (repeat 2 6 alnum))
			:display "max.mustermann@provider.com"))
    ("mac" . (:regexp (rx word-boundary
			  (repeat 2 2
				  (repeat 2 2 hex-digit) (zero-or-one (any "." ":" "-"))
				  (repeat 2 2 hex-digit) (any "." ":" "-"))
			  (repeat 2 2 hex-digit) (zero-or-one (any "." ":" "-"))
			  (repeat 2 2 hex-digit)
			  word-boundary)
		      :display "01:23:45:67:89:ab"))
    ("password" . (:regexp (concat
			    (rx-to-string
			     `(: bol (* nonl)
				 (group (| . ,password-word-equivalents))
				 (* nonl) (any . ,password-colon-equivalents)
				 (? "\^@") (* blank)))
			    "\\(.*\\)")
			   :match 2
			   :display "secretPassw#rd"))
    ("IPv4" . (:regexp (rx word-boundary
			   (repeat 1 3 digit) "." (repeat 1 3 digit) "."
			   (repeat 1 3 digit) "." (repeat 1 3 digit)
			   word-boundary)
		       :display "123.145.167.189"))
    ("IPv6" . (:regexp (rx 
			(or
			 (seq (repeat 2 2 ":")(repeat 1 4 hex-digit))
			 (seq word-boundary
			      (repeat 7 7 (repeat 1 4 hex-digit) ":")
			      (repeat 1 4 hex-digit))
			 (seq word-boundary
			      (repeat 1 6 (repeat 1 4 hex-digit) ":")
			      (repeat 1 6 ":" (repeat 1 4 hex-digit))))
			word-boundary)
		       :display "2100:fedc::ba98:7654:3210"))
    ("private ssh" . (:regexp (rx
			       (one-or-more "-")
			       "BEGIN "
			       (zero-or-more (any "A-Z"))
			       " PRIVATE KEY"
			       (one-or-more "-")
			       "\n"
			       (group (repeat 256 4096
					      (any alnum cntrl "/" "+" "=")))
			       "\n"
			       (one-or-more "-")
			       "END "
			       (zero-or-more (any "A-Z"))
			       " PRIVATE KEY"
			       (one-or-more "-"))))
    ("hash" . (:regexp (rx
     			(seq word-boundary
     			     (>= 32 hex-digit))))))
  "List of secrets to hide.  Each secret is identified by a KEYWORD string.
For each secret a property list must contain the following keyword:
:regex a regular expression to match the secret.
The following keywords are optional:
:match the matching group.Default to 0 (full match).
:display a string that is displayed instead of the secret (default:***********)."
  :group 'hide-secrets
  :type 'alist)

(defcustom hide-secrets-face
  'modus-themes-subtle-cyan
  "Face use to highlight the secrets hidden by hide-secrets."
  :group 'hide-secrets
  :type 'face)

;; functions
(defun hide-secrets--get-regex (secret)
  "Get the regex to match SECRET in the buffer."
  (eval (plist-get (cdr (assoc secret hide-secrets-alist)) :regexp)))

(defun hide-secrets--get-match (secret)
  "Get the display matching group of the regex matching SECRET to replace."
  (eval (plist-get (cdr (assoc secret hide-secrets-alist)) :match)))


(defun hide-secrets--get-display (secret)
  "Get the display string to replace SECRET in the buffer."
  (eval (plist-get (cdr (assoc secret hide-secrets-alist)) :display)))

(defun hide-secrets--hide (secret &optional start end)
  "Hide all occurrences of SECRET in buffer.
SECRET is a key in the hide-secrets-alist.

If START and END are give only hide occurences between
these points."
  (let ((rx (hide-secrets--get-regex secret))
	(match (or (hide-secrets--get-match secret) 0))
	(display  (or (hide-secrets--get-display secret) (make-string 10 ?*))))
    (save-excursion
      (goto-char (or start (point-min)))
      (while (re-search-forward rx (or end nil) t)
	(let ((overlay (make-overlay (match-beginning match) (match-end match))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display display)
	  (overlay-put overlay 'face hide-secrets-face)
	  (overlay-put overlay 'hide-secrets-type secret)
	  (overlay-put overlay 'reveal-toggle-invisible
                       #'hide-secrets--toggle))))))

(defun hide-secrets--toggle (overlay hide)
  "Toggle function for reveal mode.

OVERLAY specifies the overlay to reveal.
If HIDE is true it is reveal else it is hidden again."
  (if hide
      (hide-secrets--hide hide-secrets-current-secret
			  (overlay-start overlay)
			  (overlay-end overlay))
    (hide-secrets-show (overlay-start overlay) (overlay-end overlay))
    (let ((secret
	   (overlay-get overlay 'hide-secrets-type)))
      (setq hide-secrets-current-secret secret))))

;;; from asoc.el
(defun hide-secrets--asoc-keys (alist)
  "Return a list of unique keys in ALIST.

For a list of all keys in order, with duplicates, use `mapcar' with `car' over
ALIST."
  (let ( result
         (rest alist) )
    (while rest
      (let ((pair (car rest)))
        (unless (member (car pair) result)
	  (push (car pair) result))
        (setq rest (cdr rest))))
    (reverse result)))



;;;###autoload
(defun hide-secrets (&optional start end)
  "Hide secrets like IP addresses, passwords, email addresses etc. in the buffer.

 If START and END are given, only do so between these points."
  (interactive)
  (mapcar (lambda (secret) (hide-secrets--hide secret (or start) (or end))) (hide-secrets--asoc-keys hide-secrets-alist)))

(defun hide-secrets-show (&optional start end)
  "Remove all overlays with the `hidden-text' property in the buffer.

 If START and END are given, only do so between these points."
  (interactive)
  (dolist (overlay (overlays-in
		    (or start (point-min))
		    (or end (point-max))))
    (when (overlay-get overlay 'hidden-text)
      (delete-overlay overlay))))


;; minor mode
;;;###autoload
(define-minor-mode hide-secrets-mode
  "Minor mode to hide secrets like password, keys and IP addresses in buffers."
  :lighter " HS"
  (if hide-secrets-mode
      (progn (hide-secrets)
	     (add-hook 'post-command-hook 'hide-secrets nil t)
	     (when (equal major-mode #'eat-mode)
	       (add-hook 'eat-update-hook 'hide-secrets nil t))
	     (when (equal major-mode #'eshell-mode)
	       (add-hook 'eshell-post-command-hook 'hide-secrets nil t)))
    (hide-secrets-show)
    (remove-hook 'post-command-hook 'hide-secrets t)
    (remove-hook 'eat-update-hook 'hide-secrets t)
    (remove-hook 'eshell-post-command-hook 'hide-secrets t)))

(provide 'hide-secrets)

;;; hide-secrets.el ends here

;; Local Variables:
;; jinx-languages: "en"
;; End:
