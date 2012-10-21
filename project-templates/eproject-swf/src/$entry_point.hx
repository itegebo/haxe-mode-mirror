/*
  Thsi file was generated from template at
  $source
  by
  $generator
  If you want to change the way templates
  are generated, edit the generator file, alternatively,
  you can edit the template or create an entirely new project
  template.

  Variables available in this template:
  $$source - the source directory where templates are stored
  $$project - the name of the generated project
  $$destination - the directory where the project is created
  $$entry_point - the name to give to the entry point class
  $$package - the package of the entry_point class
  $$package_path - the package delimited by slashes rather
  than dots.
  $$package_dot - same as $$package, but followed by dot.
  $$author - the author of the project you can customize this
  through M-x costomize-mode RET haxe-mode
  $$date - the dat the project was generated
  $$generator - the program which generated the project
*/
package $package;

import flash.display.Sprite;
import flash.Lib;

/*
  @author $author
  @date $date
*/
class $entry_point extends Sprite
{
  function new()
  {
    super();
  }

  function init()
  {
    // Application entry point
  }
	
  static function main()
  {
    var o = new $entry_point();
    Lib.current.addChild(o);
    o.init();
  }
}