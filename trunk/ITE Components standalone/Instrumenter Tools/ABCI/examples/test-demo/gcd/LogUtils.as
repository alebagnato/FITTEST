package gcd {
  import flash.events.Event; 
  
  public class LogUtils {  
    public static function logFCallEntry(fName:String, ...args) {
      var log_message:String = "FUNCTION CALL: " + fName + "(";
      for (var i:uint = 0; i < args.length; i++) {
	  log_message += args[i] + ":" + (typeof (args[i]) + ",");
      }
      log_message = log_message.substr(0, log_message.length - 1) + ")"
      trace (log_message);
    }

    public static function logFCallExit(fName:String, fRes, ...args) {
      var log_message:String = "FUNCTION CALL RESULT: " + fName + "(";
      for (var i:uint = 0; i < args.length; i++) {
	  log_message += args[i] + ":" + (typeof (args[i]) + ",");
      }
      log_message = log_message.substr(0, log_message.length - 1) + ") = " + fRes;
      trace (log_message);
    }

    public static function logIf(branch:Boolean, cond:String) {
      if (branch) {
	trace ("THEN branch was taken: " + cond + " = TRUE");
      }
      else {
	trace ("ELSE branch was taken: "+ cond + " = FALSE");
      }
    }

    public static function logWhile(fName:String, iterCount:int) {
      trace(fName + " count of iteration: "+iterCount);
    }

//     public static function logIf_(branch:Boolean, ...args) {
//       var log_message:String = "";
//       if branch {
// 	log_message = "THEN branch was taken: ";
//       }
//       else {
// 	log_message = "ELSE branch was taken: ";
//       }
//       for (var i:uint = 0; i < args.length; i++) {
// 	  log_message += args[i] + ":" + (typeof (args[i]) + ",");
//       }
//     }

    public static function logEvent(evt:Event) {
      var log_message:String = "EVENT " + evt.type + " in " + evt.currentTarget;
      trace (log_message);
    }
}
}