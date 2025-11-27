;;; dune-transient.el --- Transient menu for OCaml Dune build system  -*- lexical-binding: t; -*-

;; Author: Gemini
;; Version: 2.0
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

(defvar dune-transient--saved-flags nil
  "Stores flags (profile, watch, etc.) passed from the main menu to the subcommand menu.")

(defvar dune-transient--active-command nil
  "Stores the active command ('build' or 'runtest') for the target menu.")

(defvar dune-transient--target-history nil
  "History for custom dune targets.")

;;; --- Helpers ---

(defun dune-transient--get-root ()
  "Attempt to find the dune-project root, fallback to default-directory."
  (or (locate-dominating-file default-directory "dune-project")
      default-directory))

(defun dune-transient--extract-target (args)
  "Find the ::target= argument in ARGS, return its value, and return the rest."
  (let* ((target-arg (cl-find-if (lambda (x) (string-prefix-p "::target=" x)) args))
         (target (when target-arg (substring target-arg 9))) ;; Remove "::target="
         (clean-args (cl-remove-if (lambda (x) (string-prefix-p "::target=" x)) args)))
    (cons target clean-args)))

(defun dune-transient--compose-command (subcommand flags target)
  "Construct the final shell string for compile."
  (let ((cmd (concat "dune " subcommand " " (mapconcat 'identity flags " "))))
    (if (and target (not (string-empty-p target)))
        (concat cmd " " target)
      cmd)))

;;; --- Target Infix ---

(defclass dune-transient-target-option (transient-option)
  ((key :initform "t")
   (argument :initform "::target=")
   (description :initform "Target")
   (reader :initform 'dune-transient--read-target))
  "Class for the target pseudo-argument.")

(defun dune-transient--read-target (prompt initial history)
  (read-string "Target (empty for default, . for current): " 
               nil 'dune-transient--target-history))

(transient-define-argument dune-transient-infix-target ()
  :class 'dune-transient-target-option)

;;; --- Sub-Menu: Target Selection ---

(transient-define-prefix dune-transient-target-menu ()
  "Sub-menu for setting the target and executing."
  [:description
   (lambda ()
     (format "Configure %s" (propertize (or dune-transient--active-command "Build") 'face 'transient-heading)))
   
   ["Configuration"
    (dune-transient-infix-target)]
   
   ["Execute"
    ("RET" "Run" dune-transient-exec-final)
    ;; Shortcuts to match the command name for smoother workflow (b -> b, t -> t)
    ("b" "Run (Build)" dune-transient-exec-final :if (lambda () (string-equal dune-transient--active-command "build")))
    ("t" "Run (Test)"  dune-transient-exec-final :if (lambda () (string-equal dune-transient--active-command "runtest")))]])

(transient-define-suffix dune-transient-exec-final ()
  "Execute the final command with saved flags and current target."
  (interactive)
  (let* ((target-args (transient-args 'dune-transient-target-menu))
         (target-pair (dune-transient--extract-target target-args))
         (target (car target-pair))
         ;; We ignore other args from target menu, assuming only target is there.
         ;; Combine saved flags (from main menu) with the target.
         (root (dune-transient--get-root))
         (default-directory root)
         (cmd (dune-transient--compose-command 
               dune-transient--active-command 
               dune-transient--saved-flags 
               target)))
    (compile cmd)))

;;; --- Main Menu Suffixes ---

(transient-define-suffix dune-transient-prepare-build ()
  "Capture flags and open Build target menu."
  :description "Build..."
  :key "b"
  (interactive)
  (setq dune-transient--saved-flags (transient-args 'dune-transient))
  (setq dune-transient--active-command "build")
  (dune-transient-target-menu))

(transient-define-suffix dune-transient-prepare-test ()
  "Capture flags and open Test target menu."
  :description "Test..."
  :key "t"
  (interactive)
  (setq dune-transient--saved-flags (transient-args 'dune-transient))
  (setq dune-transient--active-command "runtest")
  (dune-transient-target-menu))

(transient-define-suffix dune-transient-clean ()
  "Run 'dune clean' immediately."
  :description "Clean"
  :key "c"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune clean")))

(transient-define-suffix dune-transient-install ()
  "Run 'dune install' immediately."
  :description "Install"
  :key "i"
  (interactive)
  (let* ((args (transient-args 'dune-transient))
         (default-directory (dune-transient--get-root))
         (cmd (concat "dune install " (mapconcat 'identity args " "))))
    (compile cmd)))

(transient-define-suffix dune-transient-promote ()
  "Run 'dune promote' immediately."
  :description "Promote"
  :key "x"
  (interactive)
  (let ((default-directory (dune-transient--get-root)))
    (compile "dune promote")))

;;; --- Main Menu Definition ---

;;;###autoload
(transient-define-prefix dune-transient ()
  "Main Transient menu for Dune."
  [:description
   (lambda ()
     (concat 
      (propertize "Dune" 'face 'transient-heading)
      "\n"
      (propertize (format "Root: %s" (dune-transient--get-root)) 'face 'font-lock-comment-face)))
   
   ["Global Flags"
    ("-a" "Auto Promote" "--auto-promote")
    ("-w" "Watch mode"   "--watch")
    ("-f" "Force"        "--force")]
   
   ["Configuration"
    ("-p" "Profile" "--profile=" :choices ("dev" "release") :always-read t)]]

  ["Actions"
   [("b" "Build..." dune-transient-prepare-build)
    ("t" "Test..."  dune-transient-prepare-test)]
   
   [("c" "Clean" dune-transient-clean)
    ("i" "Install" dune-transient-install)
    ("x" "Promote" dune-transient-promote)]])

(provide 'dune-transient)
;;; dune-transient.el ends here
