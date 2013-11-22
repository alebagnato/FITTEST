package {
  import flash.display.Sprite
  import test.Test;
  import test.test1.*;

  public class ImportExample extends Sprite{
    
    public function doTrace():void{
      trace("foo")
    }
  
    public function doNotTrace():void {
    }

    public function test(x:String):void {
    }
    
    public function ImportExample() {
      var t:test.Test = new test.Test();
      var t1:SubPackage = new SubPackage();
      
    }
  }
}

class OutOfPackage {
  internal var new_var:String;
}