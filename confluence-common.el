;;; confluence-common.el --- Common utility code used by confluence and confluence-edit

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: James Ahlborn <james@boomi.com>
;; Keywords: 
;; Version: 1.5-beta
;; Package-Requires: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;; Various and asundry utility methods used by confluence and confluence-edit
;;
;; DOWNLOADING
;;
;; This module is available at Google Code:
;;
;;   http://code.google.com/p/confluence-el/
;;

;;; Code:


;; this is never set directly, only defined here to make the compiler happy
(defvar confluence-completing-read nil)


(defmacro with-quiet-rpc (&rest body)
  "Execute the forms in BODY with `url-show-status' set to nil."
  `(let ((url-show-status nil))
     ,@body))
  
(defun cf-get-page-anchors ()
  "Gets the anchors in the current page."
  (let ((anchors nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "{anchor:\\([^{}\n]+\\)}" nil t)
        (push (cons (match-string 1) t) anchors))
      ;; headings are also implicit anchors
      (goto-char (point-min))
      (while (re-search-forward "^h[1-9][.]\\s-+\\(.+?\\)\\s-*$" nil t)
        (push (cons (match-string 1) t) anchors)))
    anchors))

(defun cf-read-string (prompt-prefix prompt hist-alist-var hist-key 
                       comp-func-or-table &optional
                       require-match init-val def-val)
  "Prompt for a string using the given prompt info and history alist."
  ;; we actually use the history var as an alist of history vars so we can
  ;; have different histories in different contexts (e.g. separate space
  ;; histories for each url and separate page histories for each space)
  (let ((hist-list (cf-get-struct-value (symbol-value hist-alist-var) 
                                        hist-key))
        (result-string nil))
    (setq result-string
          (cf-read-string-simple (concat (or prompt-prefix "") prompt)
                                 'hist-list comp-func-or-table
                                 require-match init-val def-val))
    ;; put the new history list back into the alist
    (cf-set-struct-value hist-alist-var hist-key hist-list)
    result-string))

(defun cf-read-string-simple (prompt hist-list-var comp-func-or-table
                              &optional require-match init-val def-val)
  "Prompt for a string using the given prompt info and history list."
  (let ((current-completions nil)
        (current-other-completions nil)
        (last-comp-str nil)
        (completion-buffer (or (and (boundp 'completion-buffer)
                                    completion-buffer)
                               (current-buffer)))
        (confluence-completing-read t))
    (with-quiet-rpc
     ;; prefer ido-completing-read if available
     (if (and (fboundp 'ido-completing-read)
              (listp comp-func-or-table))
         (ido-completing-read prompt (mapcar 'car comp-func-or-table) nil require-match init-val hist-list-var def-val)
       (completing-read prompt comp-func-or-table
                        nil require-match init-val hist-list-var def-val t)))))

(defun cf-read-char (prompt allowed-chars-regex &optional def-char)
  "Prompt for a character using the given PROMPT and ALLOWED-CHARS-REGEX.
If DEF-CHAR is given it will be returned if user hits the <enter> key."
  (let ((the-char nil))
    (while (not the-char)
      (setq the-char (char-to-string (read-char-exclusive prompt)))
      (if (not (string-match allowed-chars-regex the-char))
          (if (and def-char (string-equal (char-to-string ?\r) the-char))
              (setq the-char def-char)
            (setq the-char nil))))
    the-char))
  
(defun cf-complete (comp-str pred comp-flag comp-table)
  "Executes completion for the given args and COMP-TABLE."
  (cond
   ((not comp-flag)
    (or (try-completion comp-str comp-table pred) comp-str))
   ((eq comp-flag t)
    (or (all-completions comp-str comp-table pred) (list comp-str)))
   ((eq comp-flag 'lambda)
    (and (assoc comp-str comp-table) t))))


(defun cf-result-to-completion-list (result-list key)
  "Translates the rpc result list into a list suitable for completion."
  (mapcar
   '(lambda (el)
      (cons (cf-get-struct-value el key) t))
   result-list))

(defun cf-get-struct-value (struct key &optional default-value)
  "Gets a STRUCT value for the given KEY from the given struct, returning the
given DEFAULT-VALUE if not found."
  (or (and struct
           (cdr (assoc key struct)))
      default-value))

(defun cf-set-struct-value-copy (struct key value)
  "Copies the given STRUCT, sets the given KEY to the given VALUE and returns
the new STRUCT."
  (let ((temp-struct (copy-alist struct)))
    (cf-set-struct-value 'temp-struct key value)
    temp-struct))

(defun cf-set-struct-value (struct-var key value)
  "Sets (or adds) the given KEY to the given VALUE in the struct named by the
given STRUCT-VAR."
  (let ((cur-assoc (assoc key (symbol-value struct-var))))
    (if cur-assoc
        (setcdr cur-assoc value)
      (add-to-list struct-var (cons key value) t))))

(defun cf-string-notempty (str)
  "Returns t if the given string is not empty."
  (> (length str) 0))

(defun cf-string-empty (str)
  "Returns t if the given string is empty."
  (= (length str) 0))



(provide 'confluence-common)
;;; confluence-common.el ends here
