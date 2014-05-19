;;; lambda-mode.el --- Inferior Lambda inside Emacs

;; Copyright (c) 2014 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; Maintainer: Joost Kremers <joostkremers@fastmail.fm>
;; Created: 2014
;; Version: 0.01
;; Keywords: lisp lambda

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Lambda is a toy Lisp-like language based on the book "Build Your Own
;; Lisp" (http://www.buildyourownlisp.com/). lambda-mode.el implements a
;; simple comint-mode for it.

;;; Code:

;;; lambda-mode for Lambda source files

(put 'fn 'doc-string-elt 2)
(put 'mac 'doc-string-elt 2)
(put 'def 'doc-string-elt 3)
(put 'var 'doc-string-elt 3)

(define-derived-mode lambda-mode lisp-mode "Λ"
  "Major mode for editing Lambda source files.

\\<lambda-mode-map>")

;;; Inferior Lambda

(defvar inferior-lambda-exec-path "/home/joost/src/lambda/lambda"
  "Path to the program used by `run-lambda'.")
 
(defvar inferior-lambda-arguments '()
  "Commandline arguments to pass to `lambda'")
 
(defvar inferior-lambda-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-lambda'")
 
(defvar inferior-lambda-prompt-regexp "lambda>"
  "Prompt for `run-lambda'.")

(defun run-lambda ()
  "Run an inferior instance of `lambda' inside Emacs."
  (interactive)
  (let ((lambda-program inferior-lambda-exec-path)
        (buffer (comint-check-proc "Lambda")))
    ;; pop to the "*Lambda*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'lambda-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Lambda*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Lambda" buffer
             lambda-program inferior-lambda-arguments)
      (lambda-mode))))

(defun inferior-lambda-initialize ()
  "Helper function to initialize Lambda."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-lambda-mode comint-mode "Lambda"
  "Major mode for `run-lambda'.

\\<inferior-lambda-mode-map>"
  nil "Lambda"
  ;; this sets up the prompt so it matches: lambda>
  (setq comint-prompt-regexp inferior-lambda-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  ;; (set (make-local-variable 'font-lock-defaults) '(lambda-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) inferior-lambda-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'inferior-lambda-mode-hook 'inferior-lambda-initialize)

(provide 'lambda-mode)

;;; lambda-mode ends here
