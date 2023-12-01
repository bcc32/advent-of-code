;;; advent-of-code.el --- Useful shortcuts for Advent of Code  -*- lexical-binding: t; -*-
;;
;; Version: 0.1.0
;; Homepage: https://github.com/bcc32/advent-of-code.el
;; Package-Requires: ((emacs "25.1") (request "0.3.0"))
;;
;;; Commentary:
;;
;; This package provides some convenient features for solving Advent of Code
;; problems quickly.
;;
;; Press C-c C-c in an input buffer ("aoc.in") to re-download the actual input
;; assigned to you, in case you edited the file for debugging purposes.
;;
;; After the output buffer ("aoc.out") is written to and auto-reverted by Emacs,
;; its contents are automatically copied into the kill ring (and the X
;; clipboard, if supported).
;;
;; Alternatively, pressing C-c C-c in an output buffer will revert the buffer
;; and submit the contents automatically, prompting for the level number (1 or
;; 2) and confirmation.
;;
;; Before using this package, you must customize `advent-of-code-cookie-jar' and
;; `advent-of-code-email'.
;;
;;; Code:

(require 'request)
(require 'seq)

(defgroup advent-of-code nil
  "Customization group for advent-of-code."
  :group 'emacs)

(defcustom advent-of-code-year (nth 5 (decode-time))
  "The year of the Advent of Code event.  Defaults to the current year."
  :type 'integer)

(defcustom advent-of-code-cookie-jar nil
  "File containing cookies for adventofcode.com.

Used to download input for the authenticated user."
  :type '(choice file (const nil)))

(defcustom advent-of-code-email nil
  "Email address to send with User-Agent header string.

The Advent of Code team requests[0] that the User-Agent string
include your contact information.  This value will also be sent
as the HTTP From header.

[0]: https://www.reddit.com/r/adventofcode/comments/z9dhtd/please_include_your_contact_info_in_the_useragent/
"
  :type 'string)

(defun advent-of-code--request (day endpoint &rest rest)
  "Make an HTTP request to adventofcode.com for day DAY, endpoint ENDPOINT.

Pass REST to `request'."
  (unless advent-of-code-email
    (user-error "`advent-of-code-email' is unset; refusing to send automated requests."))
  (let ((url (format "https://adventofcode.com/%d/day/%d/%s"
                     advent-of-code-year day endpoint))
        (request--curl-cookie-jar advent-of-code-cookie-jar))
    (apply #'request url
           :headers '(("User-Agent"
                       . (format "github.com/bcc32/advent-of-code by bcc32, used by %s"
                                 advent-of-code-email))
                      ("From" . advent-of-code-email))
           rest)))

;;;###autoload
(define-derived-mode advent-of-code-input-mode text-mode
  "AoC Input"
  "Mode for problem input buffers."
  :group 'advent-of-code
  (auto-revert-mode))

(define-key advent-of-code-input-mode-map (kbd "C-c C-c") 'advent-of-code-input-revert-to-real)

(defvar advent-of-code--problem-number nil
  "The problem number associated with this buffer.")
(make-variable-buffer-local 'advent-of-code--problem-number)

;; FIXME: call this first
(defun advent-of-code--problem-number ()
  "Return the value of `advent-of-code--problem-number', prompting if nil."
  (or advent-of-code--problem-number
      (setq advent-of-code--problem-number
            (string-to-number
             (completing-read "Problem number: " nil nil nil nil nil
                              (number-to-string (nth 3 (decode-time))))))))

(defun advent-of-code--check-cookie-jar-set-and-exists-p ()
  "Check that `advent-of-code-cookie-jar' is non-nil and points to an existing file."
  (unless (and advent-of-code-cookie-jar (file-exists-p advent-of-code-cookie-jar))
    (user-error "You must set `advent-of-code-cookie-jar' before calling this function")))

(defun advent-of-code-input-revert-to-real ()
  "Revert the current input buffer to the real input for this problem."
  (interactive)
  (advent-of-code--check-cookie-jar-set-and-exists-p)
  (advent-of-code--request
   (advent-of-code--problem-number) "input"
   :type "GET"
   :parser #'buffer-string
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (delete-region (point-min) (point-max))
               (insert data)
               (save-buffer)))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (display-message-or-buffer
              (format-message "Failed to fetch input: %s" data))))))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "aoc.in" eos) 'advent-of-code-input-mode))

;;;###autoload
(define-derived-mode advent-of-code-output-mode text-mode
  "AoC Output"
  "Mode for solution output buffers."
  :group 'advent-of-code
  (auto-revert-mode)
  (add-hook 'after-revert-hook 'advent-of-code-copy-buffer-contents-to-clipboard))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx "aoc.out" eos) 'advent-of-code-output-mode))

(defun advent-of-code-copy-buffer-contents-to-clipboard ()
  "Copy the contents of the current buffer to the clipboard or kill ring."
  (kill-new (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun advent-of-code--html-print-inner-text (tree)
  "Print the inner text for an HTML parse tree, TREE."
  (pcase tree
    ((pred stringp) (princ tree))
    (`(,(or 'comment 'script) . ,_))
    (`(,tag ,_ . ,children)
     (seq-doseq (child children)
       (advent-of-code--html-print-inner-text child)))))

(defun advent-of-code--html-find-first-article-inner-text (tree)
  "Find the first <article> tag in TREE and return its inner text."
  (pcase tree
    ((pred stringp))
    (`(,(or 'comment 'script) . ,_))
    (`(article . ,_)
     (with-output-to-string (advent-of-code--html-print-inner-text tree)))
    (`(,_ ,_ . ,children)
     (seq-some (lambda (child)
                 (advent-of-code--html-find-first-article-inner-text child))
               children))))

(defun advent-of-code-output-submit ()
  "Revert and then submit the contents of the output buffer.

Prompt for the level number (1 or 2)."
  (interactive)
  (advent-of-code--check-cookie-jar-set-and-exists-p)
  (revert-buffer :ignore-auto :noconfirm :preserve-modes)
  (let ((level (string-to-number
                (completing-read "Level number: " '("1" "2") nil :require-match))))
    (unless (yes-or-no-p (format "Submit buffer contents for level %d? " level))
      (user-error "Aborted"))
    (let ((answer (buffer-string)))
      (advent-of-code--request
       (advent-of-code--problem-number) "answer"
       :type "POST"
       :data `(("level" . ,level)
               ("answer" . ,answer))
       :parser (lambda ()
                 (advent-of-code--html-find-first-article-inner-text
                  (libxml-parse-html-region (point-min) (point-max))))
       :complete (cl-function (lambda (&key data &allow-other-keys)
                                (display-message-or-buffer data)))))))

(define-key advent-of-code-output-mode-map (kbd "C-c C-c") 'advent-of-code-output-submit)

(provide 'advent-of-code)
;;; advent-of-code.el ends here
