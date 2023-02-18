;;; dired-atool-transient.el --- Interface for atool  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; URL: https://github.com/xFA25E/dired-atool-transient
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: processes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a transient wrapper for atool in Dired modes.

;;;; Usage

;; Mark some files in Dired modes and run one of these commands:

;; `dired-atool-transient-pack': Run atool's --add (apack) command.
;; `dired-atool-transient-unpack': Run atool's --extract (aunpack) or
;; --extract-to= command.

;;;; Installation

;;;;; Package manager

;; If you've installed it with your package manager, you're done.
;; `dired-atool-transient-pack' and `dired-atool-transient-unpack' are
;; autoloaded, so you can call them right away.

;;;;; Manual

;; Put this file in your load-path, and put the following in your init file:

;; (require 'dired-atool-transient)

;;;; Credits

;; This package would not have been possible without the atool[1] program.

;;  [1] https://www.nongnu.org/atool/

;;; Code:

;;;; Requirements

(require 'dired)
(require 'subr-x)
(require 'transient)

;;;; Variables

(defvar da-archive-types
  '(("tar+gzip" ".tar.gz" ".tgz")
    ("tar+bzip" ".tar.bz" ".tbz")
    ("tar+bzip2" ".tar.bz2" ".tbz2")
    ("tar+compress" ".tar.Z" ".tZ")
    ("tar+lzop" ".tar.lzo" ".tzo")
    ("tar+lzip" ".tar.lz" ".tlz")
    ("tar+xz" ".tar.xz" ".txz")
    ("tar+7z" ".tar.7z" ".t7z")
    ("tar" ".tar")
    ("zip" ".zip")
    ("jar" ".jar" ".war")
    ("rar" ".rar")
    ("lha" ".lha" ".lzh")
    ("7z" ".7z")
    ("alzip" ".alz")
    ("ace" ".ace")
    ("ar" ".a")
    ("arj" ".arj")
    ("arc" ".arc")
    ("rpm" ".rpm")
    ("deb" ".deb")
    ("cab" ".cab")
    ("gzip" ".gz")
    ("bzip" ".bz")
    ("bzip2" ".bz2")
    ("compress" ".Z")
    ("lzma" ".lzma")
    ("lzop" ".lzo")
    ("lzip" ".lz")
    ("xz" ".xz")
    ("rzip" ".rz")
    ("lrzip" ".lrz")
    ("7zip" ".7z")
    ("cpio" ".cpio"))
  "Supported atool archive types.")

;;;; Commands

;;;###autoload (autoload 'dired-atool-transient-pack "dired-atool-transient")
(function-put 'da-pack 'command-modes '(dired-mode))
(transient-define-prefix da-pack ()
  :incompatible '(("--output=" "--format="))
  ["Flags"
   ("-f" "Allow the archive file to be overwritten" "--force")]
  ["Mandatory exclusive options"
   ("-o" "Output" "--output=" transient-read-file)
   ("-F" "Format" "--format=" da--read-archive-type)]
  ["Actions"
   ("x" "Pack" da--pack-execute)])

;;;###autoload (autoload 'dired-atool-transient-unpack "dired-atool-transient")
(function-put 'da-unpack 'command-modes '(dired-mode))
(transient-define-prefix da-unpack ()
  :incompatible '(("--subdir" "--extract-to="))
  ["Flags"
   ("-f" "Allow overwriting of local files" "--force")
   ("-D" "Always create new directory" "--subdir")]
  ["Options"
   ("-X" "Extract files to the specified directory" "--extract-to="
    transient-read-existing-directory)]
  ["Actions"
   ("x" "Unpack" da--unpack-execute)])

;;;; Functions

;;;;; Public

(defun da-call (args &optional files-to-try-to-relist)
  "Call atool with ARGS.
Try to relist FILES-TO-TRY-TO-RELIST in Dired buffers."
  (let ((buffer (generate-new-buffer " *atool*" t)))
    (with-current-buffer buffer
      (insert (format "Atool args: %S\n" args)))
    (set-process-sentinel
     (apply #'start-file-process "atool" buffer "atool" args)
     (lambda (process event)
       (if (equal "finished\n" event)
           (progn
             (message "Atool finished with success!")
             (dolist (file files-to-try-to-relist)
               (when (file-exists-p file)
                 (dired-relist-file file))))
         (dired-log (process-buffer process))
         (dired-log t)
         (message "Atool failed: %s.\nInspect %s" event dired-log-buffer))
       (kill-buffer (process-buffer process))))))

;;;;; Private

(defun da--pack-execute ()
  "Call atool with transient args from `dired-atool-transient'.

This function is for interactive use only.  If you want to call
atool, use `dired-atool-transient-call'."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let* ((args (transient-args 'da-pack))
         (in-files (dired-get-marked-files t current-prefix-arg nil nil t))
         (files-to-try-to-relist nil))

    (cond
     ((transient-arg-value "--output=" args)
      (cl-loop with result-args = (cons nil args)
               for tail-args on result-args
               for arg = (cadr tail-args)
               until (string-match-p (rx bos "--output=") arg)
               finally
               (let ((out-file (string-remove-prefix "--output=" arg)))
                 (setcdr tail-args (cons out-file (cddr tail-args)))
                 (setq args (cdr result-args))
                 (push (expand-file-name out-file) files-to-try-to-relist))))

     ((transient-arg-value "--format=" args)
      (push "--each" args)
      (let* ((archive-type (transient-arg-value "--format=" args))
             (extension (cadr (assoc archive-type da-archive-types))))
        (dolist (file in-files)
          (let ((out-file (concat (expand-file-name file) extension)))
            (push out-file files-to-try-to-relist)))))

     (t
      (user-error "No output or format were provided")))

    (da-call `("--add" ,@args ,@in-files) files-to-try-to-relist)))

(defun da--unpack-execute ()
  "Call atool with transient args from `dired-atool-transient'.

This function is for interactive use only.  If you want to call
atool, use `dired-atool-transient-call'."
  (declare (completion ignore) (interactive-only t))
  (interactive)
  (let ((args (transient-args 'da-unpack))
        (in-files (dired-get-marked-files nil current-prefix-arg nil nil t))
        (out-dir default-directory)
        (files-to-try-to-relist nil))

    (if-let ((dir (transient-arg-value "--extract-to=" args)))
        (setq out-dir (expand-file-name dir))
      (push "--extract" args))

    (dolist (file in-files)
      (catch 'continue
        (dolist (archive-type da-archive-types)
          (let ((extensions (cdr archive-type)))
            (dolist (extension extensions)
              (when (string-match-p (rx (literal extension) eos) file)
                (let* ((in-file-name (file-name-nondirectory file))
                       (in-file (expand-file-name in-file-name out-dir))
                       (out-file (string-remove-suffix extension in-file)))
                  (push out-file files-to-try-to-relist))
                (throw 'continue nil)))))))

    (da-call `(,@args "--each" ,@in-files) files-to-try-to-relist)))

(defun da--read-archive-type (prompt initial-input history)
  "Read archive type.
For PROMPT, INITIAL-INPUT and HISTORY see transient readers."
  (completing-read prompt da-archive-types nil t initial-input history))

;;;; Footer

(provide 'dired-atool-transient)

;; Local Variables:
;; read-symbol-shorthands: (("da-" . "dired-atool-transient-"))
;; End:

;;; dired-atool-transient.el ends here
