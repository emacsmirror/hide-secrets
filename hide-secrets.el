;;; hide-secrets.el --- A package for hiding IP addresses and passwords in Emacs buffers.  -*- lexical-binding: t; -*-
;; Copyright © 2020, by Sebastian Meisel

;; Author: Sebastian Meisel <sebastian.meisel@gmail.com>
;; Version: 1.1
;; Created:  November 12, 2023
;; Keywords: unix
;; Homepage: https://github.com/SebastianMeisel/journalctl-mode
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
;; This package provides functions for hiding secrets like IP addresses
;;  and passwords in Emacs buffers.

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

(defconst name-word-equivalents
  '("name" "имя" "नाम" "nombre" "nom" "nome" "নাম" "naam" "नाव" "പേര്" "பெயர்" "పేరు" "ਨਾਮ" "नाम" "নাম" "jina" "nom" "නම" "نام" "اسم" "שם" "navn" "nom" "nombre" "nafn" "نام" "nimi" "nome" "imya" "பெயர்" "الاسم" "ім'я" "اسم" "名"))


;; functions
(defun sm/hide-mac-addresses ()
  "Hide MAC addresses in the buffer."
  (interactive)
  (let ((mac-rx (rx word-boundary
                    (repeat 2 2
			    (repeat 2 2 hex-digit) (zero-or-one (any "." ":" "-"))
			    (repeat 2 2 hex-digit) (any "." ":" "-"))
		    (repeat 2 2 hex-digit) (zero-or-one (any "." ":" "-"))
		    (repeat 2 2 hex-digit)
                    word-boundary)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mac-rx nil t)
	(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "**:**:**:**:**:**"))))))


(defun sm/hide-ip-addresses ()
  "Hide IP addresses in the buffer."
  (interactive)
  (let ((ipv4-rx (rx word-boundary
                     (repeat 1 3 digit) "." (repeat 1 3 digit) "."
                     (repeat 1 3 digit) "." (repeat 1 3 digit)
                     word-boundary))
        (ipv6-rx (rx 
		  (or
		   (seq (repeat 2 2 ":")(repeat 1 4 hex-digit))
		   (seq word-boundary
			(repeat 7 7 (repeat 1 4 hex-digit) ":")
			(repeat 1 4 hex-digit))
		   (seq word-boundary
			(repeat 1 6 (repeat 1 4 hex-digit) ":")
			(repeat 1 6 ":" (repeat 1 4 hex-digit))))
		  word-boundary)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ipv4-rx nil t)
	(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "***.***.***.***"))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ipv6-rx nil t)
	(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "****:****:****::****"))))))

(defun sm/hide-email-addresses ()
  "Hide email addresses in the buffer."
  (interactive)
  (let ((email-rx (rx (one-or-more (or alnum "." "_" "-"))
		      "@"
		      (one-or-more (or alnum "." "_" "-"))
		      "." (repeat 2 6 alnum))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward email-rx nil t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'hidden-text t)
          (overlay-put overlay 'display "******@******"))))))

(defun sm/hide-names ()
  "Hide names in buffer."
  (interactive)
  (let ((pwd-rx (concat
		 (rx-to-string
		  `(: bol (* nonl)
		      (group (| . ,name-word-equivalents))
		      (* nonl) (any . ,password-colon-equivalents)
		      (? "\^@") (* blank)))
		 "\\(.*\\)")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pwd-rx nil t)
	(let ((overlay (make-overlay (match-beginning 2) (match-end 2))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "******"))))))


(defun sm/hide-passwords ()
  "Hide passwords in buffer."
  (interactive)
  (let ((pwd-rx (concat
		 (rx-to-string
		  `(: bol (* nonl)
		      (group (| . ,password-word-equivalents))
		      (* nonl) (any . ,password-colon-equivalents)
		      (? "\^@") (* blank)))
		 "\\(.*\\)")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pwd-rx nil t)
	(let ((overlay (make-overlay (match-beginning 2) (match-end 2))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "******"))))))

(defun sm/hide-private-keys ()
  "Hide private keys in buffer."
  (interactive)
  (let ((pkey-rx (rx
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
		  (one-or-more "-")
		  )))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pkey-rx nil t)
	(let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "******")
	  )))))

(defun sm/hide-hash-sums ()
  "Hide hash-sums in buffer."
  (interactive)
  (let ((hash-rx (rx
     		  (seq word-boundary
     		       (>= 32 hex-digit)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hash-rx nil t)
	(let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
	  (overlay-put overlay 'hidden-text t)
	  (overlay-put overlay 'display "******")
	  )))))

;;;###autoload
(defun sm/hide-secrets ()
  "Hide IP addresses, passwords, and email addresses in the buffer."
  (interactive)
  (sm/hide-mac-addresses)
  (sm/hide-ip-addresses)
  (sm/hide-passwords)
  (sm/hide-email-addresses)
  (sm/hide-private-keys)
  (sm/hide-hash-sums)
  )

(defun sm/show-secrets ()
  "Remove all overlays with the `hidden-text' property in the buffer."
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'hidden-text)
      (delete-overlay overlay))))

;; minor mode
;;;###autoload
(define-minor-mode hide-secrets-mode
  "Minor mode to hide secrets like password, keys and IP addresses in buffers."
  :lighter " HS"
  (if hide-secrets-mode
      (progn (sm/hide-secrets)
	     (add-hook 'post-command-hook 'sm/hide-secrets nil t)
	     (when (equal major-mode #'eat-mode)
	       (add-hook 'eat-update-hook 'sm/hide-secrets nil t))
	     (when (equal major-mode #'eshell-mode)
	       (add-hook 'eshell-post-command-hook 'sm/hide-secrets nil t)))
    (sm/show-secrets)
    (remove-hook 'post-command-hook 'sm/hide-secrets t)
    (remove-hook 'eat-update-hook 'sm/hide-secrets t)
    (remove-hook 'eshell-post-command-hook 'sm/hide-secrets t)))

(provide 'hide-secrets)

;;; hide-secrets.el ends here

;; Local Variables:
;; jinx-languages: "en"
;; End:
