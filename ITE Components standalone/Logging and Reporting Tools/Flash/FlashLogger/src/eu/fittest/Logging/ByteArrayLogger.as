/*

Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging
{
	
	import flash.utils.*;
	
	/**
	 * A concrete implementation of LoggerBase. It adds abilities to continue and 
	 * suspend logging, and to specify logging level. An instance of this logger can
     * be created through its constructor. Additionally, the class also maintains
     * a single global instance of the logger. Since Flash is single-threaded, using
     * this global instance should typically be sufficient.
	 * 
	 * <p>Logging level should be at most 1000.</p>
	 */ 
	public class ByteArrayLogger extends LoggerBase
	{
		
        /**
         * Specifies the maximum logging level (1000).
         */
		public static const MAXLEVEL:int = 1000 ;
		
		/**
		 * This specifies the minimum logging level for this logger. Logging
		 * below this level will be ignored.
		 */  
		public var MINlevel:int ;	
		private var _MINlevel:int ;
		
		public var timeStampingEnabled:Boolean = true ;
		
		/**
		 * As an additional functionality, we keep a string which keeps the last 
         * serialized application's state.
		 */
		protected var applicationAbsState : String = null ;
		public function getAppAbsState() : String { return applicationAbsState ; }

		
		/**
		 * Create an instance of this logger. When created it will be in the
		 * suspended state.
		 */ 
		public function ByteArrayLogger(minlevel:int)
		{
			super();
			if (minlevel>MAXLEVEL) minlevel=MAXLEVEL ;
			MINlevel = minlevel ;
			_MINlevel = MAXLEVEL+1 ;
			stop() ;
		}
		
		/**
		 * Singleton pattern, to hold a single global instance of the logger.
		 */
		static private var thislogger : ByteArrayLogger = null ;
		
		/**
		 * To get the single global instance of ByteArrayLogger.
		 */
		static public function getByteArrayLogger() : ByteArrayLogger {
			if (thislogger==null) {
				var minloggingLevel : int = 3 ;
				thislogger = new ByteArrayLogger(minloggingLevel) ;
			}
			return thislogger ;
		}
		
        /**
         * To stop this logger. When stopped, it will not write anything to its internal buffer.
         */
		public function stop():void {
			_MINlevel = MAXLEVEL+1 ;
		}
		
        
        /**
         * To make this logger to resume logging.
         */
		public function resume():void {
			_MINlevel = MINlevel ;
		}
        
        public function isStopped() : Boolean { return _MINlevel > MAXLEVEL ; }
		
		public function changeLoggingLevel(k : int) : void { 
           if (k > MAXLEVEL) return ;
           MINlevel = k ;
		   if (!isStopped()) resume() ;
	    }
		
		/**
         * To log the application state, as in logAppState. However, this variant allows
         * a logging level to be specified.
         */         
		public function logLAppState (level:int, appState : Object) : void 
		{
			if (level < _MINlevel) return ;
			applicationAbsState = serializeAnObject(appState) ;
			logAppState(appState) ;
		}
		
        /**
         * To log a flash event, as in logEvent. However, this variant allows
         * a logging level to be specified.
         */ 
		public function logLEvent (level:int,
								   appState:Object, 
								   event:Object) : void 
		{
			if (level < _MINlevel) return ;
			applicationAbsState = serializeAnObject(appState) ;
			logEvent(appState,event) ;
		}
		
		/**
	     * A convenience function to serialize an object using the logger's 
         * internal serializer (which is a FittestSerializer). The resulting
         * string is returned, without writting it to the log. 
	     */
		public function serializeAnObject(o : Object): String {
			var barr : ByteArray;
			serializer.reset() ; 
			serializer.resetByteArrayAndParTracker();
			serializer.serialize(o);
			barr = serializer.result;
			var st:String = barr.toString(); 
			return st;
		}
        
        /**
         * A variant of logLEvent. Here, we have to manually specify the event name, its targetted object,
         * its parameters, and also pass a pointer to the target application.
         */
        public function logLEvent_simple(level:int,
                                         eventName:String,
                                         targetID:String,
                                         appState:Object,
                                         args : Array) : void {
            if (level < _MINlevel) return ;
            var appEvent : LAppEvent = LAppEvent.getLAppEvent() ;
            appEvent.type =  eventName ;
            appEvent.targetID = targetID ;
            if (args == null) appEvent.argz = ARG0 ; else appEvent.argz = args ;            
            logEvent(appState,appEvent) ;                          
		 }
         
        static private const ARG0 : Array = new Array(0) ; 
         
        /**
         * As logFunEntry, but we can specify a logging level.
         */
		public function logLFunEntry ( 
			level:int,
			fname:Object, 
			fclassName:Object, 
			ftarget:Object,  
			...args) : void 
		{
		    //trace("*** logLFunEntry " + fclassName 
			//           + ", _MINlevel=" + _MINlevel + ", level=" + level
			//		   + ", MINlevel=" + MINlevel ) ;
			if (level < _MINlevel) return ;
			//trace("*** logLFunEntry passes level check") ;
			logFunEntry(fname,fclassName,ftarget,args) ;
		}

        /**
         * As logFunExit, but we can specify a logging level.
         */		
		public function logLFunExit( 
			level:int,
			fname:Object, 
			fclassName:Object, 
			ftarget:Object,  
			ret:*     
		) : void 
		{
			if (level < _MINlevel) return ;
			logFunExit(fname,fclassName,ftarget,ret) ;
		}
		
        /**
         * As logFunCallEntry, but we can specify a logging level.
         */
		public function logLFunCallEntry ( 
			level:int,
			callerName:Object,
			callerClass:Object,
			calleeName:Object,
			calleeClass:Object,
			ftarget:Object, 
			...args
		) : void 
		{
			if (level < _MINlevel) return ;
			logFunCallEntry(callerName,callerClass,calleeName,calleeClass,ftarget,args) ;
		}
		
        /**
         * As logFunCallExit, but we can specify a logging level.
         */
		public function logLFunCallExit ( 
			level:int,
			callerName:Object,
			callerClass:Object,
			calleeName:Object,
			calleeClass:Object,
			ftarget:Object, 
			ret:Object,
			exc:Object
		) : void 
		{ 
			if (level < _MINlevel) return ;
			logFunCallExit(callerName,callerClass,calleeName,calleeClass,ftarget,ret,exc) ;
		}
		
        /**
         * As logBlock, but we can specify a logging level.
         */		
		public function logLBlock(
			level:int,
			blockId : Object,
			funName : Object,
			className : Object) : void 
		{
			if (level < _MINlevel) return ;
			logBlock(blockId,funName,className) ;
		}
		
        /**
         * As logBlockExceptionHandler, but we can specify a logging level.
         */		
		public function logLBlockExceptionHandler(
			level:int,
			blockId : Object,
			funName : Object,
			className : Object,
			exc : Object )   // the thrown exception
			: void 
		{
			if (level < _MINlevel) return ;
			logBlockExceptionHandler(blockId,funName,className,exc) ;
		}
		
        /**
         * As logBlockLoopEnter, but we can specify a logging level.
         */		
		public function logLBlockLoopEnter(
			level:int,
			blockId : Object,
			funName : Object,
			className : Object) 
			: void 
		{
			if (level < _MINlevel) return ;
			logBlockLoopEnter(blockId,funName,className) ;
		}
		
        /**
         * As logBlockLoopExit, but we can specify a logging level.
         */		
		public function logLBlockLoopExit(
			level:int,
			blockId : Object,
			funName : Object,
			className : Object,
			loopCount : int) : void 
		{
			if (level < _MINlevel) return ;
			logBlockLoopExit(blockId,funName,className,loopCount) ;
		}
		
		
	}
}