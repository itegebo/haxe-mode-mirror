(require 'cl)

(defvar haxe-images-dir
  (concat (file-name-directory load-file-name) "etc/images/"))

(defmacro haxe-define-images (images)
  (append
   `(progn)
   (loop for image in images
         collect
         `(defimage ,(intern (concat "haxe-" image "-icon"))
            ((:type png :file
                    (concat haxe-images-dir ,(concat image ".png"))))))))

(haxe-define-images
 ("private" "public" "static" "instance" "inline"
  "volatile" "variable" "class" "interface" "macro"
  "enum" "deftype" "function"))

(provide 'ede/haxe-speedbar)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
