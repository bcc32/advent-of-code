;;; advent-of-code.el --- Useful shortcuts for Advent of Code
;;
;; Version: 0.1.0
;; Homepage: https://github.com/bcc32/advent-of-code.el
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
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
;;; Code:

(require 'request)

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

(defun advent-of-code--request (day endpoint &rest rest)
  "Make an HTTP request to adventofcode.com for day DAY, endpoint ENDPOINT.

Pass REST to `request'."
  (let ((url (format "https://adventofcode.com/%d/day/%d/%s"
                     advent-of-code-year day endpoint))
        (request--curl-cookie-jar advent-of-code-cookie-jar))
    (apply #'request url rest)))

;;;###autoload
(define-derived-mode advent-of-code-input-mode text-mode
  "AoC Input"
  "Mode for problem input buffers."
  :group 'advent-of-code
  (auto-revert-mode))

(define-key advent-of-code-input-mode-map (kbd "C-c C-c") 'advent-of-code-input-revert-to-real)

(defvar advent-of-code--input-problem-number)
(make-variable-buffer-local 'advent-of-code--input-problem-number)

(defun advent-of-code--check-cookie-jar-set-and-exists-p ()
  "Check that `advent-of-code-cookie-jar' is non-nil and points to an existing file."
  (unless (and advent-of-code-cookie-jar (file-exists-p advent-of-code-cookie-jar))
    (user-error "You must set `advent-of-code-cookie-jar' before calling this function")))

(defun advent-of-code-input-revert-to-real ()
  "Revert the current input buffer to the real input for this problem."
  (interactive)
  (advent-of-code--check-cookie-jar-set-and-exists-p)
  (unless advent-of-code--input-problem-number
    (setq advent-of-code--input-problem-number
          (string-to-number
           (completing-read "Problem number: " nil nil nil nil nil
                            (number-to-string (nth 3 (decode-time)))))))
  (advent-of-code--request
   advent-of-code--input-problem-number "input"
   :type "GET"
   :parser (lambda () (buffer-substring-no-properties (point-min) (point-max)))
   :success (cl-function (lambda (&key data &allow-other-keys)
                           (delete-region (point-min) (point-max))
                           (insert data)
                           (save-buffer)))))

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
  (kill-new (buffer-substring-no-properties (point-min) (point-max))))

(provide 'advent-of-code)
;;; advent-of-code.el ends here
