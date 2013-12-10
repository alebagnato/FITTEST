package {

import flash.display.Sprite
import flash.text.TextField
import eu.fittest.SimpleLogger

/*
  A simple Hello World example. The exercise is to inject
  the first or the the second call to the log-function in
  the function foo.
*/
public class HelloWorld extends Sprite {
   
   public var output : TextField ;
   
   public function HelloWorld() {
      output = new TextField() ;
      addChild(output) ;
      var a : Array = new Array() ;
      a[0] = "some content" ;
      output.text = "hello world" ;
      foo(this,"bla",19,a) ;
   }   
   
   public function foo(x1:Object,x2:String,x3:int,x4:Array) : void {
       // eu.fittest.SimpleLogger.logFun(this,3,"foo") ;
       // eu.fittest.SimpleLogger.logFun(this,3,"foo",x1,x2,x3,x4) ;
   }
   
   public function dummy() : void {
      eu.fittest.SimpleLogger.logFun(this,3,"dummy") ;
   }
   
   
}

}