package eu.fittest {

   import flash.text.TextField

   public class SimpleLogger {
   
      static public function logFun(
        app : Object,
        level : int,
        fname : String,
        ...args) : void {
           (app["output"] as TextField).text = "** calling " + fname + " with args: " ;
           for (var i:uint = 0 ; i < args.length; i++) {
               (app["output"] as TextField).text += args[i] + ", " ;
           }
        
        }
        
   
   }

}