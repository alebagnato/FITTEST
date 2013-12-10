package {
    import flash.display.Sprite;
    
    public class DynamicTest2 extends Sprite{
	public function DynamicTest2():void {
	    trace("DynamicTest2 constructor call")
	    var x:DClass = new DClass();
	    trace("initial toString" + x.toString());
	    x.toString = function ():int {return 3};
	    trace("new toString" + x.toString());
	    //x.foo;
	    trace(x.foo);
	}
    }
}

internal class SClass {
    //internal var foo:int;
}

internal dynamic class DClass extends SClass {
    internal var bar:int;
    //internal var foo:Function;
}
