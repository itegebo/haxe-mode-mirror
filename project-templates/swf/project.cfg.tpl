;; -*- mode: Lisp; -*-

(setq prj-config
      '(("project-name" . "$project")))

(setq prj-tools
      '(("Make" "haxe build.hxml" "f9")
        ("Clean" "rm ./bin/*.swf" "C-f9")
        ("Run" "echo run what" "f8")
        ("Stop" "-e eproject-killtool" "C-f8")))

(setq prj-files
      '(("buid.hxml" 1 1)
        ("bin/index.html" 1 1)
        ("bin/expressInstall.swf" 1 1)
        ("bin/js/swfobject.js" 1 1)
        ("src/$package/$entry_point.hx" 1 1)))

(setq prj-curfile nil)

(setq prj-functions nil)
