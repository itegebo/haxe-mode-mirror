;;; Commentary:

;; ------------------------------------------------------------------------
;; Copyright (C) Oleg Sivokon (olegsivokon@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ------------------------------------------------------------------------

;; This program is an addition to haxe-mode.el for creating and managing
;; HaXe projects. It is based on CEDET SRecode templates system
;; (http://www.randomsample.de/cedetdocs/srecode/)

;; haxe-project is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(eval-when-compile (require 'cl))
(require 'haxe-log)

(defvar build-hxml "build.hxml"
  "The name of the nxml file to build the project")

;; prj-current form eproject
(defvar project-root "./src/"
  "The name of the project source directory.
Upon load the value will be replaced with the `src' 
directory found on the path to the loaded file.
This is the best I could do for now")
(make-local-variable 'project-root)

(defvar project-build-command nil
  "This is the command passed to HaXe compiler when the project is compiled
if not specified, the script searches for it in `project-root'/`build-hxml'")

(defcustom hxtags-location
  (concat (file-name-directory load-file-name) "/hxtags.sh")
  "The program for generating TAGS files"
  :type 'string :group 'haxe-mode)

(defcustom haxe-templates
  (concat (file-name-directory load-file-name) "/project-templates/")
  "The location of project templates (this would be the directory named
project-templates under the directory where you installed haxe-mode."
  :type 'string :group 'haxe-mode)

(defcustom haxe-project-kinds
  ((lambda ()
     (remove-if
      (lambda (d)
        (or (member d '("." ".."))
            (not (file-directory-p d))))
      (directory-files haxe-templates))))
  "The kinds of supported projects, they should go with compiler target platforms
but aren't limited to it. This should correspond to the name of the directory inside
project-templates directory"
  :type 'list :group 'haxe-mode)

(defvar haxe-project-type-table (make-hash-table :test #'equal)
  "This variable contains references to all classes in currently opened project
It is a hash-table with keys being type names (strings) and values being alists
of the form (FILE-NAME . OFFSET) where FILE-NAME is the name of the source file
declaring the type and the OFFSET is the position in the file, where this type
is declared.")

(defcustom haxe-etags-program "etags"
  "This is the path (or alias) of the etags program. On Linux it is usually preinstalled
and globally available. On Windows you would probably need to install it."
  :type 'string :group 'haxe-mode)

(defcustom haxe-project-generator "generator"
  "This is the name of the program to search for in the project template directory
in order to create parts of the project haxe-mode doesn't create on its own.

The process of creating the project has 2 stages:
	1. The files are copied from the source (templates) directory to the
	   destination directory. Files with extensions `haxe-template-extension'
	   are taken to be the templates. Their extension is removed and the
	   files are processed by templates engine. Other files are simply copied
	   to the destination directory.
	2. The `haxe-project-generator' program is called (the program itself 
	   is not copied into the project directory). It is invoked in the 
	   directory where the project is created with the environment variables
	   populated with the preferences provided by the user. The rule for
	   matching between eLisp variables names and environment variables names
	   is as follows: \"shy\" (alternatively known as \"hyphen\" characters)
	   are replaced by underscores. Alphanumeric characters not altered.
	   All other characters are removed.
		There are these variables (for now):
		$src - the source of the templates directory.
		$dst - the destination directory, where project is created.
		$project - the name of the project being created.
		(more to come)
"
  :type 'string :group 'haxe-mode)

(defcustom haxe-project-args nil
  "The alist of arguments to pass to the project generator for substitution
in the project templates.
This can be either an alist, or a callable object. If it is callable, it will
be called with no arguments and it must return an alist containing key-value
pairs to substitute in the project templates upon project creation."
  :type '(or string function) :group 'haxe-mode)

(defun resolve-project-root ()
  "Used at the time of building the commands involving currnet project directory
will try first to find the value of `prj-current' (form eproject), if it doesn't
exist, will return `project-root'."
  (if (boundp 'prj-current)
    (cadr prj-current) project-root))

(defun create-haxe-tags (dir-name)
  "Create HaXe tags file."
  ;; TODO: we can use haxe -xml here to generate the XML
  ;; and parse it into TAGS file instead.
  (interactive "DDirectory: ")
  (eshell-command 
   (format "'%s' '%s'" hxtags-location dir-name)))

(defun haxe-project-generator-args (&rest available)
  "Loops over the arguments and removes pairs of arguments if the second
is NIL. This is needed so we don't pass nills to the generator."
  (loop for (key value) on available by #'cddr
        when value append (list key value)))

(defun haxe-create-project (kind project-name destination
                                 &optional entry-point package)
  "Creates HaXe project using the templates found in `haxe-templates'.
KIND is the directory containing the template files (under `haxe-templates')
PROJECT-NAME is the name to give to the new project
DESTINATION is the directory where the new project is created
  optional:
ENTRY-POINT is the name of the entry point class (and file) of the new project
  if you don't specify it, the generator will use its own judgement
PACKAGE is the package to place the entry point
 if you don't specify it, the entry point is created in the top-level package."
  (interactive
   (let ((i-kind
          (completing-read
           "What kind of project should I create? "
           haxe-project-kinds nil t
           (car haxe-project-kinds))))
     (message "Kind? %s" i-kind)
     (if (and i-kind (not (string= "" i-kind)))
         (let ((i-name (read-string "What should I call it? "))
               (i-destination
                (read-directory-name
                 "Where should I create the project? "))
               (i-entry
                (completing-read
                 "Project's entry point: " '("Main")))
               (i-package
                (read-string
                 "Package for the entry point? " nil nil "")))
           (when (or (not i-name) (string= "" i-name))
             (setq i-name "HaxeProject"))
           (when (or (not i-entry) (string= "" i-entry))
             (setq i-entry nil))
           (when (or (not i-package) (string= "" i-package))
             (setq i-package nil))
           (list i-kind i-name i-destination i-entry i-package))
       (error "Must specify a valid template"))))
  (block nil
    (when (file-exists-p destination)
      (let ((dstfiles (directory-files destination t)))
        (when (and dstfiles (< 2 (length dstfiles)))
          (unless (yes-or-no-p "New project directory is not empty, proceed anyway? ")
            (return-from nil)))))
    (let* ((src (expand-file-name (concat haxe-templates kind "/")))
           (pj-path (concat src haxe-project-generator)))
      (when (file-exists-p pj-path)
        (apply #'start-process
               (append
                (list pj-path "*haxe-project-generator*" pj-path)
                (haxe-project-generator-args
                 "-s" (expand-file-name src)
                 "-d" (expand-file-name destination)
                 "-n" project-name
                 "-e" entry-point
                 "-p" package
                 "-l" "+generator.log")))))))

(provide 'haxe-project)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-project.el ends here.
