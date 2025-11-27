;;; dune-transient.el --- Transient menu for OCaml Dune build system  -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 1.1
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

(defvar dune-transient-default-target ""
  "The current target selected in the transient menu.
Empty string means default (project root).")

;;; --- Helpers ---

(defun dune-transient--get-root ()
  "Attempt to find the dune-project root, fallback to default-directory."
  (or (locate-dominating-file default-directory "dune-project")
      default-directory))

(defun dune-transient--compose-command (subcommand args target)
  "Construct the final shell string for compile."
  ;; args contains the list of enabled flags (e.g. "--watch", "--profile=release")
  (let ((cmd (concat "dune " subcommand " " (mapconcat 'identity args " "))))
    (if (and target (not (string-empty-p target)))
        (concat cmd " " target)
      cmd)))

;;; --- Custom Infixes ---

(defclass dune-transient-target-variable (transient-variable)
  ((variable :initform 'dune-transient-default-target))
  "Class for handling the Dune target positional argument.")

(transient-define-infix dune-transient-set-target ()
  "Set the build target (positional argument)."
  :description "Target"
  :class 'dune-transient-target-variable
  :key "t"
  :argument "" ;; Required to prevent "Unbound slot: argument" error
  :reader (lambda (_prompt _history _initial)
            (let ((choice (read-string "Target (empty for default, . for current): " 
                                       nil 'dune-transient--target-history)))
              choice)))

(defun dune-transient--format-target ()
  "Display the current target in the menu info."
  (if (string-empty-p dune-transient-default-target)
      (propertize "Default (Root)" 'face 'transient-inactive-value)
    (propertize dune-transient-default-target 'face 'transient-value)))

;;; --- Suffixes (Actions) ---

(defun dune-transient--run (subcommand)
  "Run the dune SUBCOMMAND with current arguments and target."
  (interactive)
  (let* ((args (transient-args 'dune-transient))
         (root (dune-transient--get-root))
         (default-directory root)
         (cmd (dune-transient--compose-command subcommand args dune-transient-default-target)))
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

(transient-define-infix dune-transient-set-target ()
  "Set the build target (positional argument)."
  :description "Target"
  :class 'dune-transient-target-variable
  :key "T"
  :argument "" ;; Required to prevent "Unbound slot: argument" error
  :reader (lambda (_prompt _history _initial)
            (let ((choice (read-string "Target (empty for default, . for current): " 
                                       nil 'dune-transient--target-history)))
              choice)))

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
    
    ;; Custom Target Display
    ("T" (lambda () (format "Target [%s]" (dune-transient--format-target))) dune-transient-set-target)]]

  ["Actions"
   [("b" "Build" dune-transient-build)
    ("t" "Test"  dune-transient-test)]
   
   [("c" "Clean" dune-transient-clean)
    ("i" "Install" dune-transient-install)
    ("x" "Promote" dune-transient-promote)]])

(provide 'dune-transient)
;;; dune-transient.el ends here
