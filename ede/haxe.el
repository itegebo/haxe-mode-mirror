(require 'ede)
(require 'haxe-project)

(defvar haxe-ede-project-list nil
  "List of projects created by option `haxe-ede-project'.")

(defun haxe-ede-file-existing (dir)
  "Find a HaXe project in the list of HaXe projects.
DIR is the directory to search from."
  (let ((projs haxe-ede-project-list)
        (ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
        (when (string-match (concat "^" (regexp-quote root)) dir)
          (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

(defun haxe-ede-project-data (dir)
  "Load the project settings and create a new `haxe-ede-project'
from whatever we loaded.
The project file may contain set these variables:
  * name - the name of the project (mandatory)
  * version - the project version (defaults to [0 0])
  * compiler - the location of HaXe compiler (dfaults to \"haxe\")
  * std-lib - the location of Std library (defaults to nil)
  * haxelib - the location of haxelib (defaults to nil)
  * lib - the location of HaXe libraries (such as swf format, svg format etc)
  * hxml - the list of *.hxml files to use in this project. Each such file
    is assigned to a certain kind of compilation to do. Once you are working
    with the project, you can set either hxml file as a current one, so that
    when you compile, the proper file is used. This is the instrument to manage
    different compilation settings in a single project.
    By default there are `release' and `debug' targets, but you can add more
    by creating an additional hxml file and appending it to the list.
You may place arbitrary Lisp forms in your project too. The project
is loaded by `load-file' function so that whatever Emacs Lisp code
is in the project it will be executed."
  (let ((compiler "haxe")
        (version [0 0])
        (file (expand-file-name ".haxeproject" dir))
        name std-lib haxelib lib hxml)
    (load-file file)
    (unless name (error "Invalid project file, missing \"name\""))
    (haxe-ede-project
     name
     :name name
     :version version
     :directory (file-name-as-directory dir)
     :compiler compiler
     :std-lib std-lib
     :haxelib haxelib
     :file file
     :lib lib
     :hxml hxml)))

;;;###autoload
(defun haxe-ede-load (dir &optional rootproj)
  "Return a HaXe Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (haxe-ede-file-existing dir)
      ;; Doesn't already exist, so lets make one.
      (ede-add-project-to-global-list (haxe-ede-project-data dir))))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "haxe"
                       :name "haxe-ede-project"
                       :file 'ede/haxe
                       :proj-file ".haxeproject"
                       :load-type 'haxe-ede-load
                       :class-sym 'haxe-ede-project
                       :new-p t
                       :safe-p t))

(defclass haxe-ede-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'haxe-ede-project-list)
   (keybindings :initform (("R" . haxe-speedbar-resouces)))
   (menu :initform
         (["Edit Project File" ede-edit-file-target
           (ede-buffer-belongs-to-project-p)]
          "--"
          ["Update Version" ede-update-version ede-object]
          ["Version Control Status" ede-vc-project-directory ede-object]
          "--"
          ["Rescan Project Files" ede-rescan-toplevel t]))
   (compiler :initarg :compiler
            :initform "haxe"
            :type string
            :documentation "The location of HaXe compiler")
   (std-lib :initarg :std-lib
            :initform "/usr/lib/haxe/std/"
            :type string
            :documentation "The location of HaXe standard library")
   (haxelib :initarg :haxelib
            :initform "/usr/lib/haxe/haxelib/"
            :type string
            :documentation "The location of haxelib")
   (lib :initarg :lib
        :initform "/usr/lib/haxe/lib/"
        :type string
        :documentation "The location of HaXe library (swf format, svg format etc.)")
   (hxml :initarg :hxml
         :initform nil
         :type list
         :documentation "The *.hxml files registered for this project as the ways
to compile it. This should be an alist containing a (symbol . \"file.hxml\") conses."))
  "A general puropose HaXe project")

(defmethod initialize-instance ((this haxe-ede-project) &rest fields)
  "Make sure the targets slot is bound."
  (call-next-method)
  ;; This will have to deal with `hxml' slot once I understand what exactly does this do
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))
  (oset this configurations '("debug" "release"))
  (oset this configuration-default "debug")
  (when (not (file-exists-p (oref this :file)))
    (if (y-or-n-p (format "No .haxeproject file exists in %s.  Create?"
                          (file-name-directory (oref this :file))))
        (call-interactively 'haxe-create-ede-project)
      ;; Else, possible problems
      (message "You may run into problems in this project
if there is no .haxeproject file."))))

(defun haxe-speedbar-resouces ()
  (interactive)
  ;; Not sure if we need to be interactive here...
  (message "I am haxe-speedbar-resouces and I was called!"))

(provide 'ede/haxe)
