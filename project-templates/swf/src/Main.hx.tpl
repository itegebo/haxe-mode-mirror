package {{package}};
{
    import flash.display.Sprite;
    import flash.Lib;
    
    class {{main-class}} extends Sprite
    {
	function new() { }

	function init()
	{
	    // Application entry point
	}
	
	static function main()
	{
	    var o = new {{main-class}}();
	    Lib.current.addChild(o);
	    o.init();
	}
    }
}
	    