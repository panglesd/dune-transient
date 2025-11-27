;;; dune-transient.el --- Transient menu for OCaml Dune build system  -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 6.0
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
  "Stores the active command ('build' or 'runtest') for the configuration menu.")

(defvar dune-transient--target-history nil
  "History for custom dune targets.")

;;; --- Helpers ---

(defun dune-transient--get-root ()
  "Attempt to find the dune-project root, fallback to default-directory."
  (or (locate-dominating-file default-directory "dune-project")
      default-directory))

(defun dune-transient--compose-command (subcommand flags target)
  "Construct the final shell string for compile."
  (let ((cmd (concat "dune " subcommand " " (mapconcat 'identity flags " "))))
    (if (and target (not (string-empty-p target)))
        (concat cmd " " target)
      cmd)))

(defun dune-transient--run-final (target &optional specific-dir)
  "Execute the active command with current transient flags and TARGET.
If SPECIFIC-DIR is provided, the command runs in that directory.
Otherwise, it runs in the project root."
  (let* ((flags (transient-args 'dune-transient-config-menu))
         (root (dune-transient--get-root))
         (work-dir (or specific-dir root))
         (default-directory work-dir)
         (cmd (dune-transient--compose-command 
               dune-transient--active-command 
               flags 
               target)))
    (compile cmd)))

;;; --- Panel 2: Configuration & Execution Actions ---

(transient-define-suffix dune-transient-run-default ()
  "Run the command without a specific target."
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
    (dune-transient--run-final nil dir)))

(transient-define-suffix dune-transient-run-custom ()
  "Prompt for a target and run immediately."
  :description "Specify target..."
  :key "t"
  (interactive)
  (let ((target (read-string "Target: " nil 'dune-transient--target-history)))
    (dune-transient--run-final target)))

(transient-define-prefix dune-transient-config-menu ()
  "Panel 2: Configuration and Execution."
  [:description
   (lambda ()
     (format "Configure %s" (propertize (or dune-transient--active-command "Build") 'face 'transient-heading)))
   
   ["Flags"
    ("-a" "Auto Promote" "--auto-promote")
    ("-w" "Watch mode"   "--watch")
    ("-f" "Force"        "--force")]
   
   ["Configuration"
    ("-p" "Profile" "--profile=" :choices ("dev" "release") :always-read t)]
   
   ["Aliases"
    ( "a" "all"         "@all")
    ( "c" "check"       "@check")
    ( "d" "doc"         "@doc")
    ( "D" "default"     "@default")
    ( "f" "fmt"         "@fmt")
    ( "i" "index"       "@ocaml-index")
    ( "I" "install"     "@install")
    ( "r" "runtest"     "@runtest")
    ( "j" "doc-json"    "@doc-json")
    ( "n" "doc-new"     "@doc-new")
    ( "k" "pkg-install" "@pkg-install")]]

  ["Execute"
   [("b" "Run (Default)" dune-transient-run-default)
    ("." "Run in dir..." dune-transient-run-in-dir)
    ("t" "Specify Target" dune-transient-run-custom)]])

;;; --- Panel 1: Main Menu Actions ---

(transient-define-suffix dune-transient-open-build ()
  "Open the configuration panel for 'dune build'."
  :description "Build..."
  :key "b"
  (interactive)
  (setq dune-transient--active-command "build")
  (dune-transient-config-menu))

(transient-define-suffix dune-transient-open-test ()
  "Open the configuration panel for 'dune runtest'."
  :description "Test..."
  :key "t"
  (interactive)
  (setq dune-transient--active-command "runtest")
  (dune-transient-config-menu))

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
   
   ["Core Commands"
    ("b" "Build..." dune-transient-open-build)
    ("t" "Test..."  dune-transient-open-test)]

   ["Utilities"
    ("f" "Format"  dune-transient-simple-fmt)
    ("p" "Promote" dune-transient-simple-promote)
    ("c" "Clean"   dune-transient-simple-clean)
    ("i" "Install" dune-transient-simple-install)]])

(provide 'dune-transient)
;;; dune-transient.el ends here
