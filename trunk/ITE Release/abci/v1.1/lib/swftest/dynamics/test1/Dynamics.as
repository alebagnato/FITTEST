package {
    import flash.display.Sprite;

    public class Dynamics extends Sprite {
	public function Dynamics():void {
	    var dc:DynamicClass = new DynamicClass();
	    dc.foo = "bar";
	    //trace("*** test a dynamic trait presence: " + dc.foo.toString());
	}
    }
}