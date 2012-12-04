(defun haxe-validate-install ()
  "Run this function to make sure all necessary parts are
present, this will check for HaXe compiler, neko runtime
Python and Bash (Python and Bash aren't mandatory, but good
to have). Will also verify if our info files are registered
with Emacs (don't know how to do that yet)."
  )

(defun haxe-install-ac-sources ()
  "Identify the location directory of `auto-complete' mode
and add HaXe sources to where it is installed."
  )

(defun haxe-install-yasnippets ()
  "Identify the location of `yasnippets' sinppets directory
and add our snippets there."
  )

(defun haxe-install-info ()
  "Add our info files to the directories, where Emacs can
find them."
  )

;; If we get installed outside MELPA

(defun haxe-validate-full-install ()
  "Checks for the presense of necessary dependencies:
- flymake,
- cl,
- eieio,
- ede (eproject alternatively),
- semantic,
- thingatpt,
- yasnippet,
- auto-complete (try to support ido completion, as
  an alternative, not implemented yet).
and tries to install them. First thing attempted is to
install them through MELPA repositories, if MELPA is
not installed, try install MELPA first, then interactively
install all of the missing dependencies through it.
If user declines to install MELPA, but still wants to
install dependencies through this script, then offer to
download and compile them interactively, well, what we can...
Let's just hope this will not happen :).
After all this being done, will run `haxe-validate-install'."
  )

(defun haxe-post-install-config (settings)
  "After we performed various checks and accumulated information
about the environment, we also need to set up some mode-specific
constants or defaults."
  )

(defun haxe-install ()
  "Install and configure `haxe-mode' after it was loaded.
This function runs interactively and tries to either install or
fix the installation if there was one."
  (interactive)
  )
