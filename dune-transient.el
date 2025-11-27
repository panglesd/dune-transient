;;; dune-transient.el --- Transient menu for OCaml Dune build system  -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 1.2
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Keywords: ocaml, dune, build, tools

;;; Commentary:
;; This package provides a transient interface (Magit-style menu) for the
;; OCaml Dune build system.
;;
;; Usage:
;; 1. Load this file: (load "path/to/dune-transient.el")
;; 2. M-x dune-transient

;;; Code:

(require 'transient)
(require 'compile)
(require 'cl-lib)

;;; --- Custom Variables for State ---

(defvar dune-transient--target-history nil
  "History for custom dune targets.")

;;; --- Helpers ---

(defun dune-transient--get-root ()
  "Attempt to find the dune-project root, fallback to default-directory."
  (or (locate-dominating-file default-directory "dune-project")
      default-directory))

(defun dune-transient--compose-command (subcommand args)
  "Construct the final shell string for compile.
Extracts the target from ARGS (prefixed with ::target=) and removes it
from the flags list."
  (let* ((target-arg (cl-find-if (lambda (x) (string-prefix-p "::target=" x)) args))
         (target (when target-arg (substring target-arg 9))) ;; Remove "::target=" prefix
         (clean-args (cl-remove-if (lambda (x) (string-prefix-p "::target=" x)) args))
         (cmd (concat "dune " subcommand " " (mapconcat 'identity clean-args " "))))
    (if (and target (not (string-empty-p target)))
        (concat cmd " " target)
      cmd)))

;;; --- Custom Infixes ---

(transient-define-infix dune-transient-set-target ()
  "Set the build target (positional argument)."
  :description "Target"
  :key "T"
  :argument "::target="
  :reader (lambda (_prompt _history _initial)
            (let ((choice (read-string "Target (empty for default, . for current): " 
                                       nil 'dune-transient--target-history)))
              choice)))

;;; --- Suffixes (Actions) ---

(defun dune-transient--run (subcommand)
  "Run the dune SUBCOMMAND with current arguments and target."
  (interactive)
  (let* ((args (transient-args 'dune-transient))
         (root (dune-transient--get-root))
         (default-directory root)
         (cmd (dune-transient--compose-command subcommand args)))
    (compile cmd)))

(transient-define-suffix dune-transient-build ()
  "Run 'dune build'."
  :description "Build"
  :key "b"
  (interactive)
  (dune-transient--run "build"))

(transient-define-suffix dune-transient-test ()
  "Run 'dune runtest'."
  :description "Test"
  :key "t"
  (interactive)
  (dune-transient--run "runtest"))

(transient-define-suffix dune-transient-clean ()
  "Run 'dune clean'."
  :description "Clean"
  :key "c"
  (interactive)
  (dune-transient--run "clean"))

(transient-define-suffix dune-transient-install ()
  "Run 'dune install'."
  :description "Install"
  :key "i"
  (interactive)
  (dune-transient--run "install"))

(transient-define-suffix dune-transient-promote ()
  "Run 'dune promote'."
  :description "Promote"
  :key "x"
  (interactive)
  (dune-transient--run "promote"))

;;; --- The Menu Definition ---

;;;###autoload
(transient-define-prefix dune-transient ()
  "Transient menu for Dune."
  [:description
   (lambda ()
     (concat 
      (propertize "Dune Command Builder" 'face 'transient-heading)
      "\n"
      (propertize (format "Root: %s" (dune-transient--get-root)) 'face 'font-lock-comment-face)))
   
   ["Flags"
    ("-a" "Auto Promote" "--auto-promote")
    ("-w" "Watch mode"   "--watch")
    ("-f" "Force"        "--force")]
   
   ["Configuration"
    ;; The user asked for -p to choose profile. 
    ;; We provide choices: dev (default), release, or custom.
    ("-p" "Profile" "--profile=" 
     :choices ("dev" "release")
     :always-read t)
    
    ;; Target Selection
    (dune-transient-set-target)]]

  ["Actions"
   [("b" "Build" dune-transient-build)
    ("t" "Test"  dune-transient-test)]
   
   [("c" "Clean" dune-transient-clean)
    ("i" "Install" dune-transient-install)
    ("x" "Promote" dune-transient-promote)]])

(provide 'dune-transient)
;;; dune-transient.el ends here
