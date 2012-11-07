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

(defvar haxe-install-dir
  (file-name-directory
   (substring
    (file-name-directory load-file-name) 0 -1))
  "Where haxe-mode is installed, the source-files will be in the ./lisp
directory, i.e. a child directory of it")

(defvar haxe-build-hxml "build.hxml"
  "The name of the nxml file to build the project")
(make-local-variable 'haxe-build-hxml)

;; prj-directory form eproject
(defvar haxe-project-root "./"
  "The name of the project source directory.
Upon load the value will be replaced with the `src' 
directory found on the path to the loaded file.
This is the best I could do for now")
(make-local-variable 'haxe-project-root)

(defvar haxe-project-sources `(,(expand-file-name "./src/"))
  "This is a list of all sources of the current project")
(make-local-variable 'haxe-project-sources)

(defvar haxe-project-build-command nil
  "This is the command passed to HaXe compiler when the project is compiled
if not specified, the script searches for it in `haxe-project-root'/`haxe-build-hxml'")
(make-local-variable 'haxe-project-build-command)

(defcustom haxe-hxtags-location
  (concat haxe-install-dir "scripts/hxtags.sh")
  "The program for generating TAGS files"
  :type 'string :group 'haxe-mode)

(defcustom haxe-templates
  (concat haxe-install-dir "project-templates/")
  "The location of project templates (this would be the directory named
project-templates under the directory where you installed haxe-mode."
  :type 'string :group 'haxe-mode)

(defcustom haxe-project-kinds
  ((lambda ()
     (remove-if
      (lambda (d)
        (or (member d '("." ".."))
            (not (file-directory-p (concat haxe-templates "/" d)))))
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

(defvar haxe-project-generator-timer nil
  "This variable is set by the waiting timer while the projec is being generated
to the waiting timer")

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

(defun haxe-identify-project-root ()
  "Lame attempt at finding the root directory of our project.
The assumtion is that most people would call it `src', so we 
climb up the directory tree to see if there's a directory with this
name on the path and assume the last such instance to be our project
directory, for example /home/user/projects/foo/src/org/user/utils/Bar.hx
wil result in /home/user/projects/foo/src/ being selected as the root
directory"
  (setq haxe-project-root nil)
  (let* ((current (buffer-file-name))
         (pos (string-match "/src/" current))
         (dir (file-name-directory current)))
    (when (require 'ede nil 'noerror)
      ;; Let's try to search for the `ede' project first.
      (message "about to check for haxe-ede-project-of-file")
      (let ((maybe-ede-project (haxe-ede-project-of-file current)))
        (message "maybe-ede-project found? %s" maybe-ede-project)
        (when maybe-ede-project
          ;; TODO: There are more useful things we need to set here,
          ;; once we have ede project
          (setq haxe-project-root (oref maybe-ede-project directory)
                haxe-build-hxml
                (concat (oref maybe-ede-project configuration-default) ".hxml")
                haxe-compiler (oref maybe-ede-project compiler)
                haxe-project-sources (oref maybe-ede-project sources)))))
    (when (and (null haxe-project-root) (require 'eproject nil 'noerror))
      ;; In case we discover that eproject is used, first try
      ;; to find it's configuration file, perhaps if it was
      ;; created by us, it will have `prj-directory' set to
      ;; our project root. If it's not set, assume that the
      ;; directory containing the project file is the project
      ;; root. If the "project.cfg" doesn't exist, proceed
      ;; with guessing. Require the 'eproject in case someone
      ;; so we can access the variables it declares safely.
      (catch 't
        (while (position ?\/ dir)
          (when (file-exists-p (concat dir "project.cfg"))
            (if (boundp 'prj-directory)
                (progn
                  ;; (eproject-open (concat dir "project.cfg"))
                  ;; The eproject code cannot handle opening project
                  ;; files. It assumes that the project file must have been
                  ;; registered in some central repository. It is
                  ;; extremely inconvenient, and the project's own
                  ;; sources are a mess, no comments, single-letter variable
                  ;; names and so on. I don't want to waste time trying
                  ;; to figure out how to use it. This is the minimum
                  ;; I could have done. Once we have EDE this code will
                  ;; be removed.
                  (make-local-variable 'prj-directory)
                  (load-file (concat dir "project.cfg"))
                  (setq prj-directory (expand-file-name (or prj-directory dir)))
                  (throw 't (setq haxe-project-root prj-directory)))
              (throw 't nil)))
          (setq dir (file-name-directory (directory-file-name dir))))))
    (when (and (null haxe-project-root) pos)
      (setq haxe-project-root (substring current 0 pos)))
    haxe-project-root))

(defun haxe-resolve-project-root ()
  "Used at the time of building the commands involving currnet project directory
will try first to find the value of `prj-directory' (from eproject), if it doesn't
exist, will return `haxe-project-root'."
  (if (boundp 'prj-directory)
      (or prj-directory haxe-project-root)
    haxe-project-root))

(defun haxe-create-haxe-tags (dir-name)
  "Creates HaXe tags file."
  ;; TODO: we can use haxe -xml here to generate the XML
  ;; and parse it into TAGS file instead.
  (interactive "DDirectory: ")
  (eshell-command 
   (format "%s %s" (shell-quote-argument haxe-hxtags-location)
           (shell-quote-argument (expand-file-name dir-name)))))

(defun haxe-project-generator-args (&rest available)
  "Loops over the arguments and removes pairs of arguments if the second
is NIL. This is needed so we don't pass nills to the generator."
  (loop for (key value) on available by #'cddr
        when value append (list key value)))

(defun haxe-load-project (directory)
  "Makes project's directory current and loads the project from project.cfg
file in the specified directory.
This function is bound to \\[haxe-load-project]"
  (interactive "DProject directory: ")
  (unless (string=
           (expand-file-name directory)
           directory)
    (setq directory (expand-file-name directory)))
  (cd-absolute directory)
  (if (require 'eproject nil t)
      (progn                            ; I don't know... I don't feel
                                        ; like encouraging using eproject :/
                                        ; once we have some CEDET integration
                                        ; let's just move to EDE and forget it.
                                        ; Actually, screw it, it's a Pandora
                                        ; box. If you went so far as this comment
                                        ; just don't use eproject - will make
                                        ; you life easier :)
        (make-local-variable 'prj-directory)
        (when (fboundp 'prj-loadconfig)
          (prj-loadconfig
           (list "maybe-project-name?"
                 directory
                 (concat directory "project.cfg"))))
        ;; (assert (and (boundp 'prj-curfile) prj-curfile)
        ;;         nil "prj-curfile did not exist after the project was loaded")
        (find-file (if (and (boundp 'prj-curfile) prj-curfile)
                       (car prj-curfile) directory)))
    (let (prj-config prj-tools prj-files prj-curfile prj-functions)
      ;; We'll load this file pretending its been loaded by eproject
      ;; and simply display the `prj-curfile'. I'm not even sure it's
      ;; not the project file. If that's not set for w/e reason, just
      ;; display the contents of the directory we've just created.
      ;; Some insanity goes on with these directories. It's just broken
      ;; don't use it.
      (defvar prj-directory)
      (make-local-variable 'prj-directory)
      (load-file "./project.cfg")
      (find-file
       (if prj-curfile
           (if (consp prj-curfile)
               (car prj-curfile) prj-curfile))
       directory))))

(defun haxe-wait-generator-finished (directory)
  "A timer to wait until the generato process finishes to load up
the generated project"
  (when haxe-project-generator-timer
    (cancel-timer haxe-project-generator-timer))
  (message "Waiting to open project directory <%s>" directory)
  (if (get-buffer-process "*haxe-project-generator*")
      ;; The generator is still creating the project
      (setq haxe-project-generator-timer
            (run-at-time
             1 nil
             #'haxe-wait-generator-finished directory))
    (haxe-load-project directory)))

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
 if you don't specify it, the entry point is created in the top-level package.
This function is bound to \\[haxe-create-project]"
  (interactive
   (let ((i-kind
          (or (when (boundp '*dynamic-project-kind)
                *dynamic-project-kind)
              (completing-read
               "What kind of project should I create? "
               haxe-project-kinds nil t
               (car haxe-project-kinds)))))
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
      (if (file-exists-p pj-path)
          (progn
            ;; It may happen here that we are trying to call on `start-process'
            ;; in a non-existing directory, in which case Emacs will error,
            ;; let's just move into our generator directory, that one must
            ;; certainly exist
            (cd-absolute (file-name-directory pj-path))
            (apply #'start-process
                   (append
                    (list pj-path "*haxe-project-generator*" pj-path)
                    (haxe-project-generator-args
                     "-s" (expand-file-name src)
                     "-d" (expand-file-name destination)
                     "-n" project-name
                     "-e" entry-point
                     "-p" package
                     "-l" "+generator.log"))))
        (error "Project generator script does not exist in <%s>" pj-path))
      (haxe-wait-generator-finished (expand-file-name destination)))))

;;;###autoload
(defun haxe-create-ede-project (kind)
  "Creates an EDE project from a corresponding project template. Most of what this
function does is handled by the `haxe-create-project' function, this one only
determines the kind of the project.
This function is bound to \\[haxe-create-project]"
  (interactive
   ;; Need a dedicated variable for ede-project kinds, but will do for now.
   (list
    (completing-read
     "Project's entry point: " '("ede-swf") nil t "ede-sfw")))
  (let ((*dynamic-project-kind kind))
    (call-interactively #'haxe-create-project)))

(provide 'haxe-project)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-project.el ends here.
