/*

Authors: Wishnu Prasetya, Alexander Elyasov

Copyright 2012 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/
package eu.fittest.Logging.replay {

  import eu.fittest.actionscript.automation.*;

  import flash.events.* ;
  import flash.external.ExternalInterface;
  import flash.net.URLLoader ;
  import flash.net.URLRequest ;
  import flash.events.TimerEvent;
  import flash.utils.Timer;
  
  /**
   * Utility to replay a log; work for non-movie like GUI. The class also exports some
   * functions as externalized functions APIs.
   */
  public class LogReplay {
  
     /**
      * Log to replay.
      */
     public var log : XML ;
     
     /**
      * The function to use to execute an event. It should have the
      * signature of f(objectID:String, action:String, ... args).
      */
     public var executeEvent : Function = Automation.instance.invoke ;
	 
	 private static var instance : LogReplay = null ;
	 
	 public function LogReplay() { ; }
	 
	 public static function getLogReplay() : LogReplay {
	    if (instance==null) { instance = new LogReplay() ;
		                      externalizeAPIs() ;
						    }
		return instance ;
	 }
	 
	 /**
      * To externalize APIs to JavaScript.
	  */
	static protected function externalizeAPIs() : void {
		    if(ExternalInterface.available) {
			   ExternalInterface.addCallback("replayLog", getLogReplay().replayLog); 
               ExternalInterface.addCallback("replay", getLogReplay().replay);
		    }
			else trace("** Fail to set LogReplay's JavaScript APIs because ExternalInterface is not present...") ;
	}
	 
     
     /**
      * To load a FITTEST-xml log file, and to replay it.
      */
     public function replayLog(filename:String) : void {
       var xmlLoader:URLLoader = new URLLoader();
       xmlLoader.addEventListener(
         Event.COMPLETE, 
         function (e:Event) : void { 
           log = new XML(e.target.data) ;
           replay() ; 
         }
       ) ;
       xmlLoader.load(new URLRequest(filename));
     }
     
     
     private var replayStepTimer : Timer ;
     private var i = 0 ;
     /**
      * To replay the log. This assumes the log has been read.
      */
     protected function replay() : void {
        trace("** Start replaying a log... (" + log.E.length() + ")") ;
        i = 0 ;
        if (replayStepTimer==null) {
           replayStepTimer = new Timer(1000,1) ;
           replayStepTimer.addEventListener(TimerEvent.TIMER,replay1) ;
           replayStepTimer.start() ;
		}
        else {
           replayStepTimer.reset() ;
           replayStepTimer.start() ;
        }        
     }
     
     // execute the i-th event
     private function replay1(ev : Event) : void {
     
       if (i>=log.E.length()) {
          trace("** Replay done. " + i + " events were executed.") ;
          replayStepTimer.stop() ;
          return ;
       }
       trace("** Replaying event no-" + i + ":") ;
       var event : XML = log.E[i] 
       try {
         var target: String = getTargetID(event) ;
         if (target==null) {
            trace("** Replay fails at " + i + "-th event; cannot get the target-id.") ;
            replayStepTimer.stop() ;
            return ;
         }
         var eventName : String = getEventName(event) ;
         if (eventName==null) {
            trace("** Replay fails at " + i + "-th event; cannot get the event-type.") ;
            replayStepTimer.stop() ;
            return ;
         }
         var args : Array = getArgs(event) ;
         // pffft cannot use flex-num of args if the fucntion has been passed on
         // dynamically
         if (args==null || args.length==0)
              executeEvent(target,eventName) ;
         else {
            trace("## about to replay " + eventName + " on " + target + " with " + args) ;
            /*switch (args.length) {
               case 1 : executeEvent(target,eventName,args[0]) ; break ;
               case 2 : executeEvent(target,eventName,args[0],args[1]) ; break ;
               case 3 : executeEvent(target,eventName,args[0],args[1],args[2]) ; break ;
               case 4 : executeEvent(target,eventName,args[0],args[1],args[2],args[3]) ; break ;
               case 5 : executeEvent(target,eventName,args[0],args[1],args[2],args[3],args[4]) ; break ;
               // ok we'll just do it up to 5...
               default : executeEvent(target,eventName,args) ; 
            }*/
            executeEvent(target,eventName,args) ; // by urueda (all the Flash delegates takes an array of arguments)
         }
       }
       catch(e : Error) {
              trace("** Replay fails at " + i + "-th event; due to an exception: " + event) ;
              replayStepTimer.stop() ;
              throw e ;   
       }
       i++ ;
       replayStepTimer.reset() ;
       replayStepTimer.start() ;
     }
     
     /**
      * To extract the target-ID of the event represented by a given entry 
      * in the log.
      */    
     public function getTargetID(entry:XML) : String {
           var id : String = entry.O[0].fd[1].V[0].@v ;
           if (id=="null" || id=="undefined") return null ;
           return id.substr(1, id.length-2) ;
     }
       
     /**
      * To extract the event-name from a given entry in the log.
      * This entry represents a high level event.
      */
     public function getEventName(entry:XML) : String {
           var n : String = entry.O[0].fd[2].V[0].@v ;
           if (n=="null" || n=="undefined") return null ;
           return n.substr(1,n.length-2) ;
     }
  
     /**
      * To get the arguments of a high-level event.
      */
     public function getArgs(entry : XML) : Array {
          if (entry.O[0].fd[3].O[0].fd.length() <= 1) return null ;
          var args : Array = new Array() ;
          var i : int = 0 ;
          for each (var x : XML in entry.O[0].fd[3].O[0].fd) {
             var val : String = x.V[0].@v
             var ty : String = x.V[0].@ty
             if (i>0) {
                if (ty == "String") args[i-1] = val.substr(1,val.length-2) ;
                else args[i-1] = val ; 
             }
             i++ ;
          }
          return args ;
    }
  
  
  }
  
}