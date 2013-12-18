/*

Authors: Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.
*/
package eu.fittest.Logging {

  import eu.fittest.Logging.*;
  import eu.fittest.Logging.Serialization.*;
  import eu.fittest.Logging.replay.* ;
  import eu.fittest.actionscript.automation.*;

  import flash.display.*;
  import flash.events.*;
  import flash.external.ExternalInterface;
  import flash.utils.*;
  import flash.events.EventDispatcher;
  import flash.utils.*;



  /**
   * This class is a template for the hook for FittestLogger. It should be subclassed
   * by the actual hook. Such a hook is later loaded by a special AutomationLoader.
   * The effect is that an instance of FittestLogger will be attached to a target
   * application. The hook needs to specify how the application should be serialized,
   * which the logger needs to know in order to log its state. It should also specify
   * other serialization delegates, as needed.
   */
  public class FittestLoggerHook extends Sprite {
  
    /**
     * The constructor is the hook. This will kick in the whole 
	 * sequence that will attach an instance of FittestLogger 
	 * to the main application and configure it.
	 *
	 * <p>Note: automation delegates are NOT configured here. They are
	 * bundled as a separate swc and loaded by the AutomationLoader.
	 * this bundle determines which delegates are used. </p>
     */ 
    public function FittestLoggerHook() {		
      trace("** Entrering FittestLoggerHook constructor; about to setup the hook...");
      Automation.instance.addEventListener(InitEvent.INIT,setup_) ;
    }
    	
    /**
     * Pointer to the logged app.
     */ 
    public var loggedApp : Object = null ; // DisplayObject ;
		
    /**
     * A rather complicated setup function; it actually waits until the 
     * application has been loaded, the fires up the actual logger setup
     * function. Among other things, it needs to set the variable loggedApp to
     * point to the target application.
     */
    private function setup_(e : InitEvent) : void {
        if(e.root['application']) setup(e) ;
        else {
           var hasDoneSetup : Boolean = false ;
           e.root.addEventListener(Event.ADDED, 
              function waitForApp(other:Event):void {
                if(e.root['application']) {
                    if (!hasDoneSetup) setup(e) ;
                    hasDoneSetup = true ;
					e.root.removeEventListener(Event.ADDED,waitForApp);
                }
              }) ;
        }
    }
				
    private var mylogger : FittestLogger ;
				
    /** 
     * The logger setup sequence.
     */ 
    public function setup(e : InitEvent) : void {
        trace("** Main APP is loaded, starting logger setup...") ;			

        loggedApp = e.root['application'] ;
        trace("** Pointer to the application grabbed.") ;
        
        registerSerializationDelegates() ;
	    trace("** Serialization delegates are now registered.") ;
        
        // setting the logger to use:
        mylogger = FittestLogger.getFittestLogger() ;
        mylogger.MINlevel = 1 ; // we'll start the logger with deep loging to be turned on 
        trace("** FITTEST-logger created, with level=" + mylogger.MINlevel) ;
		mylogger.resume() ;
		if (mylogger.isStopped()) trace("** The logger is initialized at the stopped-state.") ;
		else trace("** The logger is initialized at the resumed-state.")  ;
		
        Automation.instance.addEventListener(RecordEvent.RECORD,interceptor);
        Automation.instance.startRecording();
        trace("** The logger is hooked to Flash event system...") ;    
        trace("** Logger is ready...") ;
        
        var replayEngine : LogReplay = LogReplay.getLogReplay() ;
        trace("** Replay-engine hooked.") ;

        setupMoreExternalInterface(loggedApp) ;
        trace("** External-interfaces hooked.") ;
    }

	/**
     * Override this to set up more external interface.
     */
	public function setupMoreExternalInterface(loggedApp : Object) : void { ; }
       
    /**
     * We set Flash events to be logged at level 5.
     */     
    static public  const LOGGING_LEVEL_OF_APPEVENTS : int = 5 ;	
    
    /**
     * The function to be used to intercept Flash events; it will
     * log the event.
     */
    public function interceptor(evt:RecordEvent):void {
        if (!ignoreList(evt)) {
           mylogger.logLEvent(LOGGING_LEVEL_OF_APPEVENTS,loggedApp,evt) ;
           mylogger.logLBlock(LOGGING_LEVEL_OF_APPEVENTS,-1,"time","bogus") ;
        }
    }
      
    /**
     * This is used to implement an ignore-list; events mathing this function (if it returns true) will be 
     * ignored/not logged. Override this as needed.
     */
    public function ignoreList(evt : RecordEvent) : Boolean {  
        return false ;
    }

    /**
     * To register serialization delegates, including a delegate to serialize the target
     * application. Override this function; please call super in the overriding function.
     */ 
    public function registerSerializationDelegates() : void {
        // registering some standard serialization delegates:
        Delegates.registerDelegate(Array,SomeSerializationDelegates.arraySerializationDelegateFunction) ;
        Delegates.registerDelegate(RecordEvent, EventSerializableDelegates.recordEventserializationDelegate) ;
    }
   
  }	
}