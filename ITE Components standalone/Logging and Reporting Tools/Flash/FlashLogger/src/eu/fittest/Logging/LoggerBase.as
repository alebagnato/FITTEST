/*

Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging {

	import eu.fittest.Logging.Serialization.*;
	
	import flash.utils.*;

/**
 * This class provides the main APIs to log flash-events, function
 * calls, etc. It provides only a partial implementation of these
 * APIs, in particular the logic controlling the format of the
 * produced log-strings. It does not for example take care of the
 * actual writing of the produced logs to a storage medium, nor does
 * it have any concept of logging level. These aspects should be taken
 * care in a refining subclass.
 * 
 * <p>The produced log-format is the format of raw FITTEST-log.</p>
 */   
  	public class LoggerBase {  

    /**
     * A variable to hold the log.
     */
	public var log : ByteArray ;
    
    /**
     * The serializer to use. By default, it is an instance of FittestSerializer,
     * so that objects are serialzied in the FITTEST format.
     */
	public var serializer : FittestSerializer ;
	
	public function LoggerBase(){
		log = new ByteArray() ;
		serializer = new FittestSerializer() ;
	}
	
    /**
     * Write a string to the log.
     */
	protected function write(s:String):void{
		log.writeUTFBytes(s) ;
	}

    /**
     * Write a bytearray to the log.
     */	
	protected function writeBytes(s:ByteArray):void{
		log.writeBytes(s) ;
	}
	
	
	protected function writeLine(s : String) : void { 
		log.writeUTFBytes(s) ;
		log.writeUTFBytes("\n") ;
	}
	
    /**
     * To serialize an object or value to the log. By default it will be
     * serialized in the FITTEST format.
     */
	protected function serialize(o : *) : void {
	// move this to the beginning of every log-function so that
        // each log entry gets IDs with respect to that entry
        // serializer.reset() ; 
		serializer.resetByteArrayAndParTracker() ;
		serializer.serialize(o) ;
		writeBytes(serializer.result) ; 
	}

    /**
     * To get a pointer to the log.
     */
	public function getResult():ByteArray {
      return log ;
    }
	
    /**
     * To retrieve the log, as a string.
     */
	public function getResultAsString():String {
      return log.toString() ;
    }
		
	/**
	 * Clearing the logger's internal buffer.
	 */ 
	public function clearBuffer():void {
	  // log.clear() ;  DOES NOT WORK on Flex compiler 3.6 ... turning it off
	  // log = new ByteArray() ;
      log.clear() ;
    }
	
    /**
     * To write a time-stamp to the log.
     */
	protected function timeStamp() : void {
		var D:Date = new Date() ;
		write(" " + D.timezoneOffset + ":" + D.valueOf()) ;
	}
	
	
    /**
	 * This is used to log the state of the target application. Pass a
     * pointer to the application, or an object that can act as a
     * representation of its state, to this function.
     */
	protected function logAppState (appState : Object) : void 
	{
	  serializer.reset() ; 
	  write("%<S") ; 
	  timeStamp() ;
	  writeLine(" \"S\"") ;
	  serialize(appState) ;
	  writeLine(CLOSE) ;
	}
    
	private static const CLOSE : String = "%>" ;
       
    /**
	 * For logging a flash-event (e.g. user clicks on a button).
     */
	protected function logEvent (appState:Object, event:Object) : void 
	{
	  serializer.reset() ; 
	  write("%<S") ; 
	  timeStamp() ;
	  writeLine(" \"E\"") ;
	  serialize(event) ;
	  serialize(appState) ;
	  writeLine(CLOSE) ;
	}
    
	
    /**
	 * For logging at a function's entry.
	 * 
	 * @param fname the name of the function that we enter.
	 * 
	 * @param  ftarget the target object, on which the function fname is called. 
	 * This target object is never null, unless fname is a static function. 
    */
	protected function logFunEntry ( 
        fname:Object, 
        fclassName:Object, 
        ftarget:Object,  
        args:Array) : void 
	{ 
	  serializer.reset() ; 
	  write("%<S") ; 
	  timeStamp() ;
	  writeLine(" \"FE:" + fname + ":" + fclassName + "\"") ;
	  serialize(ftarget) ;
	  writeLine(SecOParams) ;
	  if (args === null) { }
	  else {        
	    for (var i:uint = 0; i < args.length; i++) {
	      serialize(args[i]) ;
	    }
	  }
	  writeLine(CLOSE) ;
	  writeLine(CLOSE) ;
	}
    
    private static const SecOParams : String  = "%<S \"args\"" ;
    
	/**
	 * For logging at the point just before exiting a function.
	 */ 
	protected function logFunExit( 
        fname:Object, 
        fclassName:Object, 
        ftarget:Object,  
        ret:*     
        ) : void 
    {
      serializer.reset() ; 
	  write("%<S") ; 
	  timeStamp() ;
	  writeLine(" \"FX:" + fname + ":" + fclassName + "\"") ;
      serialize(ftarget) ;
      serialize(ret) ;
	  writeLine(CLOSE) ;
    }
	

	/**
	 * For logging a function call, just before the call is entered.
	 * 
	 * @param callerName name of the function that calls.
	 * @param calleeName name of the function that is called.
	 * @param callerClass name of the caller class.
	 * @param calleeName  name of the callee class.
	 * @param ftarget target object; null if callee is a static function.
	 */ 
	protected function logFunCallEntry ( 
		callerName:Object,
		callerClass:Object,
		calleeName:Object,
		calleeClass:Object,
		ftarget:Object, 
		args:Array
		) : void 
	{ 
        serializer.reset() ; 
		write("%<S") ; 
		timeStamp() ;
		writeLine(" \"FCE:" + callerName + ":" + callerClass + "\"") ;
		writeLine("%<P %<{ " + calleeName + ":" + calleeClass + " }%> %>") ;
		serialize(ftarget) ;
		writeLine(SecOParams) ;
		if (args === null) { }
		else {        
			for (var i:uint = 0; i < args.length; i++) {
				serialize(args[i]) ;
			}
		}
		writeLine(CLOSE) ;
		writeLine(CLOSE) ;
	}

	
	/**
	 * For logging a function call, just after the call returns.
	 * 
	 * @param callerName name of the function that calls.
	 * @param calleeName name of the function that is called.
	 * @param callerClass name of the caller class.
	 * @param calleeName  name of the callee class.
	 * @param ftarget target object; null if callee is a static function.
	 * @param ret return value.
	 * @param exc exception thrown; null otherwise.
	 */ 
	protected function logFunCallExit ( 
		callerName:Object,
		callerClass:Object,
		calleeName:Object,
		calleeClass:Object,
		ftarget:Object, 
		ret:Object,
		exc:Object
	) : void 
	{ 
        serializer.reset() ; 
		write("%<S") ; 
		timeStamp() ;
		writeLine(" \"FCX:" + callerName + ":" + callerClass + "\"") ;
		writeLine("%<P %<{ " + calleeName + ":" + calleeClass + " }%> %>") ;
		serialize(ftarget) ;
		serialize(ret) ;
		serialize(exc) ;
		writeLine(CLOSE) ;
	}
	
	
	/**
	 * To log when a certain control is entered.
	 * 
	 * @param blockId the id of the block.
	 * @param funName the function that contains the block.
	 */ 
	protected function logBlock(
           blockId : Object,
           funName : Object,
           className : Object) : void 
    {
        serializer.reset() ; 
		write("%<S") ; 
		timeStamp() ;
		write(" \"B:" + blockId + ":" + funName + ":" + className + "\" ") ;
	    writeLine(CLOSE) ;
    }
    
	/**
	 * To log when an exception handler is entered.
	 * 
	 * @param exc the exception that was thrown.
	 */ 
	protected function logBlockExceptionHandler(
           blockId : Object,
           funName : Object,
           className : Object,
           exc : Object )   // the thrown exception
           : void 
    {
       serializer.reset() ; 
	   write("%<S") ; 
	   timeStamp() ;
	   writeLine(" \"BEH:" + blockId + ":" + funName + ":" + className + "\"") ;
       serialize(exc) ;
	   writeLine(CLOSE) ;
    }
    
	/**
	 * To log when a loop is enterred (when the first iteration is about to begin).
	 */ 
	protected function logBlockLoopEnter(
		blockId : Object,
		funName : Object,
		className : Object) 
		: void 
	{
        serializer.reset() ; 
		write("%<S") ; 
		timeStamp() ;
		write(" \"BLE:" + blockId + ":" + funName + ":" + className + "\"") ;
		writeLine(CLOSE) ;
	}
	
	/**
	 * To log when a loop is exited.
	 */ 
	protected function logBlockLoopExit(
           blockId : Object,
           funName : Object,
           className : Object,
           loopCount : int) : void 
    {
       serializer.reset() ; 
	   write("%<S") ; 
	   timeStamp() ;
	   writeLine(" \"BLX:" + blockId + ":" + funName + ":" + className + "\"") ;
       writeLine("%<P %<{ cnt=" + loopCount + " }%> %>") ;
	   writeLine(CLOSE) ;
    }

    
}
}

  
  
