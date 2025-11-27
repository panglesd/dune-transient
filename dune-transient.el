;;; dune-transient.el --- Transient menu for OCaml Dune build system  -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 9.0
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

;;; --- Global State ---

(defvar dune-transient--active-command nil
  "Stores the active command ('build' or 'runtest').")

(defvar dune-transient--active-menu nil
  "Stores the symbol of the active transient menu to pull args from.")

(defvar dune-transient--target-history nil
  "History for custom dune targets.")

;;; --- Helpers ---

(defun dune-transient--get-root ()
  "Attempt to find the dune-project root, fallback to default-directory."
  (or (locate-dominating-file default-directory "dune-project")
      default-directory))

(defun dune-transient--extract-target (args)
  "Find the ::target= argument in ARGS, return (target . clean-args)."
  (let* ((target-arg (cl-find-if (lambda (x) (string-prefix-p "::target=" x)) args))
         (target (when target-arg (substring target-arg 9))) ;; Remove "::target=" prefix
         (clean-args (cl-remove-if (lambda (x) (string-prefix-p "::target=" x)) args)))
    (cons target clean-args)))

(defun dune-transient--compose-command (subcommand flags target)
  "Construct the final shell string for compile."
  (let ((cmd (concat "dune " subcommand " " (mapconcat 'identity flags " "))))
    (if (and target (not (string-empty-p target)))
        (concat cmd " " target)
      cmd)))

(defun dune-transient--run-final (&optional specific-dir)
  "Execute the active command with current transient flags and configured target.
If SPECIFIC-DIR is provided, the command runs in that directory.
Otherwise, it runs in the project root."
  (let* ((raw-args (transient-args dune-transient--active-menu))
         ;; Separate the pseudo-argument ::target= from actual flags
         (arg-pair (dune-transient--extract-target raw-args))
         (target (car arg-pair))
         (flags (cdr arg-pair))
         (root (dune-transient--get-root))
         (work-dir (or specific-dir root))
         (default-directory work-dir)
         (cmd (dune-transient--compose-command 
               dune-transient--active-command 
               flags 
               target)))
    (compile cmd)))

;;; --- Infixes ---

(defun dune-transient--read-target (prompt initial _history)
  (read-string prompt initial 'dune-transient--target-history))

(transient-define-argument dune-transient-infix-target ()
  :description "Target"
  :class 'transient-option
  :key "t"
  :argument "::target="
  :reader 'dune-transient--read-target)

;;; --- Common Execution Suffixes ---

(transient-define-suffix dune-transient-run-default ()
  "Run the command in the default context (root)."
  :description (lambda () (format "Run (Default)" )) 
  :key "b"
  (interactive)
  (dune-transient--run-final nil))

(transient-define-suffix dune-transient-run-in-dir ()
  "Prompt for a directory and run the command there."
  :description "Run in directory..."
  :key "."
  (interactive)
  (let ((dir (read-directory-name "Run in directory: " default-directory default-directory t)))
    (dune-transient--run-final dir)))

;;; --- Panel 3: Test Menu ---

(transient-define-prefix dune-transient-test-menu ()
  "Panel 3: Test Configuration."
  [:description "Test Configuration"
   
   ["Flags"
    ("-a" "Auto Promote" "--auto-promote")
    ("-w" "Watch mode"   "--watch")]
   
   ["Configuration"
    (dune-transient-infix-target)]

   ["Execute"
    ("b" "Run" dune-transient-run-default)
    ("." "Run in dir..." dune-transient-run-in-dir)]])

;;; --- Panel 2: Build Configuration ---

(transient-define-prefix dune-transient-config-menu ()
  "Panel 2: Build Configuration and Execution."
  [:description "Build Configuration"
   
   ["Flags"
    ("-a" "Auto Promote" "--auto-promote")
    ("-w" "Watch mode"   "--watch")
    ("-f" "Force"        "--force")]
   
   ["Configuration"
    ("-p" "Profile" "--profile=" :choices ("dev" "release") :always-read t)
    (dune-transient-infix-target)]
   
   ["Aliases"
    ( "D" "default"     "@default")
    ( "c" "check"       "@check")
    ( "r" "runtest"     "@runtest")
    ( "I" "install"     "@install")
    ( "a" "all"         "@all"         :level 5)
    ;; Advanced Aliases (Level 5 - Hidden by default. Show with C-x l)
    ( "d" "doc"         "@doc")
    ( "f" "fmt"         "@fmt")
    ( "i" "index"       "@ocaml-index")
    ( "j" "doc-json"    "@doc-json"    :level 5)
    ( "n" "doc-new"     "@doc-new"     :level 5)
    ( "k" "pkg-install" "@pkg-install" :level 5)]]

  ["Execute"
   [("b" "Run" dune-transient-run-default)
    ("." "Run in dir..." dune-transient-run-in-dir)]])

;;; --- Panel 1: Main Menu Actions ---

(transient-define-suffix dune-transient-open-build ()
  "Open the configuration panel for 'dune build'."
  :description "Build Config..."
  :key "B"
  (interactive)
  (setq dune-transient--active-command "build")
  (setq dune-transient--active-menu 'dune-transient-config-menu)
  (dune-transient-config-menu))

(transient-define-suffix dune-transient-open-test ()
  "Open the configuration panel for 'dune runtest'."
  :description "Test Config..."
  :key "T"
  (interactive)
  (setq dune-transient--active-command "runtest")
  (setq dune-transient--active-menu 'dune-transient-test-menu)
  (dune-transient-test-menu))

(transient-define-suffix dune-transient-quick-build ()
  "Run 'dune build' immediately."
  :description "Quick Build"
  :key "b"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune build")))

(transient-define-suffix dune-transient-quick-test ()
  "Run 'dune runtest' immediately."
  :description "Quick Test"
  :key "t"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune runtest")))

(transient-define-suffix dune-transient-quick-watch ()
  "Run watch mode on default and index."
  :description "Watch Mode"
  :key "w"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune build -w @default @ocaml-index")))

(transient-define-suffix dune-transient-simple-fmt ()
  "Run 'dune fmt' immediately."
  :description "Format (dune fmt)"
  :key "f"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune fmt")))

(transient-define-suffix dune-transient-simple-promote ()
  "Run 'dune promote' immediately."
  :description "Promote"
  :key "p"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune promote")))

(transient-define-suffix dune-transient-simple-clean ()
  "Run 'dune clean' immediately."
  :description "Clean"
  :key "c"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune clean")))

(transient-define-suffix dune-transient-simple-install ()
  "Run 'dune install' immediately."
  :description "Install"
  :key "i"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune install")))

;;; --- Panel 1: Main Menu Definition ---

;;;###autoload
(transient-define-prefix dune-transient ()
  "Main Transient menu for Dune."
  [:description
   (lambda ()
     (concat 
      (propertize "Dune Operations" 'face 'transient-heading)
      "\n"
      (propertize (format "Root: %s" (dune-transient--get-root)) 'face 'font-lock-comment-face)))
   
   ["Quick Actions"
    ("B" "Build" dune-transient-quick-build)
    ("T" "Test"  dune-transient-quick-test)
    ("W" "Watch" dune-transient-quick-watch)]

   ["Configuration"
    ("b" "Build Config..." dune-transient-open-build)
    ("t" "Test Config..."  dune-transient-open-test)]

   ["Utilities"
    ("f" "Format"  dune-transient-simple-fmt)
    ("p" "Promote" dune-transient-simple-promote)
    ("c" "Clean"   dune-transient-simple-clean)
    ("i" "Install" dune-transient-simple-install)]])

(provide 'dune-transient)
;;; dune-transient.el ends here
