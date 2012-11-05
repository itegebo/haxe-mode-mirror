(require 'cl)
(require 'haxe-project)

(defmacro haxe-define-images (images)
  (append
   `(progn)
   (loop for image in images
         with images-root = (concat haxe-install-dir "etc/images/")
         collect
         `(defimage ,(intern (concat "haxe-" image "-icon"))
            ((:type png :file
                    ,(concat images-root image ".png")))))))

(haxe-define-images 
 ("private" "public" "static" "instance" "inline"
  "volatile" "variable" "class" "interface" "macro"
  "enum" "deftype" "function"))

(provide 'ede/haxe-speedbar)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
