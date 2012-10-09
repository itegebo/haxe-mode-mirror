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
(require 'srecode)                      ; not sure about this one

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

(defcustom haxe-project-kinds '("swf")
  "The kinds of supported projects, they should go with compiler target platforms
but aren't limited to it. This should correspond to the name of the directory inside
project-templates directory"
  :type 'list :group 'haxe-mode)

(defcustom haxe-templates
  (concat (file-name-directory load-file-name) "/project-templates/")
  "The location of project templates (this would be the directory named
project-templates under the directory where you installed haxe-mode."
  :type 'string :group 'haxe-mode)

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

(defcustom haxe-template-extension ".tpl"
  "The extension of the template files in the project, the contents of templates
is red by Emacs and translated using SRecode templates engine."
  :type 'string :group 'haxe-mode)

;; (declare-function srecode-create-dictionary "srecode/dictionary")
;; (declare-function srecode-dictionary-set-value "srecode/dictionary")
;; (declare-function srecode-load-tables-for-mode "srecode/find")
;; (declare-function srecode-table "srecode/find")
;; (declare-function srecode-template-get-table "srecode/find")
;; (declare-function srecode-insert-fcn "srecode/insert")
;; (declare-function srecode-resolve-arguments "srecode/insert")
;; (declare-function srecode-map-update-map "srecode/map")

;; This doesn't work, only confuses the compilation

;;;;###autoload
;; (defun haxe-srecode-setup ()
;;   "Update various paths to get SRecode to identify our macros."
;;   (let* ((lib (locate-library "haxe-project.el" t))
;; 	 (haxedir (file-name-directory lib))
;; 	 (tmpdir (file-name-as-directory
;; 		  (expand-file-name "templates" haxeedir))))
;;     (when (not tmpdir)
;;       (error "Unable to location HaXe Templates directory"))

;;     ;; Rig up the map.
;;     (require 'srecode-map)
;;     (add-to-list 'srecode-map-load-path tmpdir)
;;     (srecode-map-update-map t)
    
;;     ;; (srecode-load-tables-for-mode 'autoconf-mode)
;;     ;; (srecode-load-tables-for-mode 'autoconf-mode 'ede)
;;     ))

;; (defmacro haxe-srecode-insert-with-dictionary (template &rest forms)
;;   "Insert TEMPLATE after executing FORMS with a dictionary.
;; TEMPLATE should specify a context by using a string format of:
;;   context:templatename
;; Locally binds the variable DICT to a dictionary which can be
;; updated in FORMS."
;;   `(let* ((dict (srecode-create-dictionary))
;; 	  (temp (srecode-template-get-table
;; 		 (srecode-table)
;; 		 ,template nil 'haxe-tpl)))
;;      (when (not temp)
;;        (error "HaXe template %s for %s not found!"
;; 	      ,template major-mode))
;;      (srecode-resolve-arguments temp dict)

;;      ;; Now execute forms for updating DICT.
;;      (progn ,@forms)
;;      (srecode-insert-fcn temp dict)))

;; ;;;###autoload
;; (defun haxe-srecode-insert (template &rest dictionary-entries)
;;   "Insert at the current point TEMPLATE.
;; TEMPLATE should specify a context by using a string format of:
;;   context:templatename
;; Add DICTIONARY-ENTRIES into the dictionary before insertion.
;; Note: Just like `srecode-insert', but templates found in 'ede app (what is this?)."
;;   (haxe-srecode-insert-with-dictionary
;;    template
;;    ;; Add in optional dictionary entries.
;;    (while dictionary-entries
;;      (srecode-dictionary-set-value
;;       dict
;;       (car dictionary-entries)
;;       (cadr dictionary-entries))
;;      (setq dictionary-entries
;; 	   (cddr dictionary-entries)))))

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

(defun haxe-process-template (old-file new-file)
  (with-temp-buffer
    (find-file old-file)
    ;; xxx-mode - current editing mode
    ;; yyy is probably the table associated with this module?
    ;; (srecode-load-tables-for-mode 'xxx-mode)
    ;; (srecode-load-tables-for-mode 'xxx-mode 'yyy)
    ;; (haxe-srecode-insert "xxx-mode:file" some-list...)
    (write-file new-file)
    (kill-buffer)))

(defun haxe-replace-template-tokens (file-name)
  (with-output-to-string
    (dotimes (i (length file-name))
      ;; that's for now, this will handle path expansions some time...
      (princ (char-to-string (aref file-name i))))))

(defun haxe-build-env-vars (src dst name)
  (with-output-to-string
    (princ (format "src='%s' dst='%s' project='%s' " src dst name))))

(defun haxe-create-project (kind project-name destination)
  "Creates HaXe project using the templates found in `haxe-templates'
by running `haxe-create-project-from-directory'."
  (interactive
   (list
    (completing-read "What kind of project should I create? "
                     haxe-project-kinds)
    (read-string "What should I call it? ")
    (read-directory-name "Where should I create the project? ")))
  (block nil
    (when (file-exists-p destination)
      (let ((dstfiles (directory-files destination t)))
	(when (and dstfiles (< 2 (length dstfiles)))
	  (unless (yes-or-no-p "New project directory is not empty, proceed anyway? ")
	    (return-from nil)))))
    (let ((src (expand-file-name (concat haxe-templates kind "/"))))
      (haxe-create-project-from-directory src destination)
      (let ((pj-path (concat src haxe-project-generator)))
        (when (file-exists-p pj-path)
          (let ((command
                 (concat "env "
                         (haxe-build-env-vars
                          (expand-file-name src)
                          (expand-file-name destination)
                          project-name) " "
                          (shell-quote-argument pj-path))))
            (haxe-log 3 "Calling env with: %s" command)
            (shell-command command)))))))

(defun haxe-create-project-from-directory (src dst)
  "Creates the files required for the project in DST from template  in SRC."
  (interactive "DTemplates directory: \nGDirectory for new project: ")
  (let ((dst (file-name-directory dst)))
    (mapc
     #'(lambda (x)
         (when (and (not (equal "." (file-name-nondirectory x)))
                    (not (equal ".." (file-name-nondirectory x))))
           (let ((new-file
                  (concat dst (haxe-replace-template-tokens
                               (file-name-nondirectory x)))))
             (if (file-directory-p x)
                 (progn
                   (make-directory (haxe-file-name-to-directory new-file) t)
                   (haxe-create-project-from-directory
                    (haxe-file-name-to-directory x)
                    (haxe-file-name-to-directory new-file)))
               (let ((x-length (length x))
                     (ext-lengt (length haxe-template-extension)))
                 (if (and (> x-length ext-lengt)
                          (string= (substring x (- x-length ext-lengt))
                                   haxe-template-extension))
                     (haxe-process-template
                      x (substring new-file 0 (- ext-lengt)))
                   (unless (string= (file-name-nondirectory x)
                                    haxe-project-generator)
                     (make-directory (file-name-directory new-file) t)
                     (copy-file x new-file))))))))
     (directory-files src t))))

(defun haxe-file-name-to-directory (file-name)
  ;; They deprecated and now removed directory-sep-char :( wtf?
  (if (char-equal (aref file-name (1- (length file-name))) ?\/)
      file-name
      (concat file-name (char-to-string ?\/))))

(provide 'haxe-project)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-project.el ends here.
