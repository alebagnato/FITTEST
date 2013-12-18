/*

Authors: Alexander Elyasov, Arie Middelkoop, Wishnu Prasetya

Copyright 2011 Utrecht University

The use of this sofware is free under the Modified BSD License.

*/

package eu.fittest.Logging
{
	
	import flash.events.*;
	import flash.net.Socket;
	import flash.net.XMLSocket;
	import flash.system.Security;
	import flash.utils.*;
	import flash.external.ExternalInterface; // by urueda

	/**
	 * This is a concrete implementation of LoggerBase that writes the log to
	 * a so-called FITTEST agent. This agent runs outside the logger as a 
     * separate program. The logger communicates with it through sockets.
     * This agent is controlled by an instance of FITTEST ITE, e.g. to allow
     * the ITE to remotely collect logs. In principle it is possible to write
     * your own way to remotely retrieving logs, by implementing the same protocol
     * between the logger and the agent.
     *
     * <p>This logger extends ByteArrayLogger, and concequently
	 * inherits its functionalities. With resect to ByteArrayLogger, the only thing we 
     * need to change is its basic write-functions,
	 * so that they now write to the socket towards the FITTEST agent. In other words,
     * the log is now maintained in the agent.
     * In addition, this class needs of course to implement the protocol to facilitate
     * communication with the agent. </p>
     *
     * <p>From ByteArrayLogger, this class actually also inherits an internal buffer where
     * a ByteArrayLogger normally keeps the log. This is now a bit redundant, since in
     * a FittestLogger the log is kept at the agent. However, it may still be 
     * convenient to keep this internal buffer, e.g. to allow the client itself to collect 
     * logs. This redundancy can be turned on and off with
     * the flag keepInternalCopy.</p>
	 * 
     * <p>We will also export/externalize a set functions to the JavaScript level. These
     * are functions to control and query the logger, so that these can be done from
     * JavaScript.</p>
	 */ 
	public class FittestLogger extends ByteArrayLogger
	{
	
		/**
		 * If true, strings written to the log will also be written to the logger 
         * internal buffer.
		 */
		protected var keepInternalCopy : Boolean = true ;
		
        /**
         * Return the status of the keepInternalCopy flag.
         */
		public function isInternalCopyKept() : Boolean { return keepInternalCopy ; }
        
        /**
         * Set or unset the flag keepInternalCopy.
         */
		public function echoLogToInternalCopy(yes : Boolean)  : void { keepInternalCopy = yes ; }
				
		/**
		 * Is true when the fittest agent is ready to receive logs;
		 * else false.
		 */ 
		protected var fittestAgentCanAcceptLog : Boolean ;
		
		/**
		 * This is used to store log data while the fittest agent is not ready.
		 * by urueda
		 */
		protected var tmpLogBuffer : ByteArray;
			
        /**
         * Protocol state with respect to the FITTEST agent. Possible states are:
         * neverBeenIntialized, initialized, started, uploading (by urueda), stopped, terminated.
         */
        private var protocolState : uint = 0 ;
        
        static private const NEVER_BEEN_INITIALIZED : uint = 0 ;
        static private const INITIALIZED : uint = 1 ;
        static private const STARTED : uint = 2 ;
        static private const STOPPED : uint = 3 ;
        static private const TERMINATED : uint = 4 ;
        static private const UPLOADING : uint = 5 ; // by urueda
        
		// a whole bunch of variabes to support the protocol with the FITTEST agent
		
        /**
         * Lower bound of the port-number to scan, to find where the agent is.
         */
		static protected const AGENT_PORT:uint = 37600;
        
        /**
         * Upper bound of the port-number to scan, to find where the agent is.
         */
		static protected const UNTIL:uint = 37650;
        
        /**
         * Port scan increment.
         */
		static protected const BY:uint = 1;

		/**
		 * This is a socket to be used for exchanging control messages with the
		 * FITTEST agent.
		 */
		protected var commandSocket:XMLSocket;
		
		/**
		 * This is the socket to be used for logging.
		 */ 
		private var _binarySocket: Socket = null;
		
		private var _currentPort:Number;
		private var _currentSocket:XMLSocket;
		
		private var _componentId:String;
		private var _componentDir:String;
		private var _started:DataEvent = null;  // can be removed??
		private var _agentId:String;
		private var _iteId:String;
			
		private var _logFileName : String ;
		private var _logTime : Number;
		private var _logPeriod : int = 0; // by urueda
		private var _logActivity : Boolean = false; // by urueda

		static protected function xtrace(u:String) : void { trace("** " + u) ; }
		
		// Constructor ...
		public function FittestLogger(minlevel:int) {
			
			super(minlevel) ;
			
			fittestAgentCanAcceptLog = false ;
			tmpLogBuffer = new ByteArray(); // by urueda
			_currentPort = AGENT_PORT;
			if(Security.sandboxType == Security.REMOTE){
				Security.loadPolicyFile("xmlsocket://localhost:"+_currentPort);
			}
			xtrace("Trying to connect to agent...") ;
			_currentSocket = new XMLSocket();
			registerHandlers(_currentSocket).connect("localhost", _currentPort);
			if(ExternalInterface.available) {			
			  ExternalInterface.addCallback("window.onunload",unloadSUT); // by urueda
			}
			else xtrace("Fail to hook on-unload handler because ExternalInterface is not present...") ;
			xtrace("An instance of FittestLogger is created...") ;
		}
		
		/**
		 * This will be called when the application in closed. The method will
         * e.g. de-register the logger from the agent.
		 * by urueda
		 */
		public function unloadSUT():void {
		    xtrace("SUT is unloaded...") ;
			if (commandSocket != null) {
	            commandSocket.send(createMessage("Deregister", _agentId, new Array("id", "towards"), new Array(_componentId, _iteId))); // by urueda		
			}
		}
		    			
		/**
		 * Singleton pattern, to hold a single global instance of FITTEST logger.
		 */
		static private var thislogger : FittestLogger = null ;
		
		/**
		 * To get the single global instance of FITTEST logger.
		 */
		static public function getFittestLogger() : FittestLogger {
			if (thislogger==null) {
				var minloggingLevel : int = 3 ;
				thislogger = new FittestLogger(minloggingLevel) ;
				externalizeAPIs(thislogger) ;
			}
			return thislogger ;
		}
		
		/**
         * To externalize APIs to JavaScript.
		 */
		static protected function externalizeAPIs(logger : FittestLogger) : void {
		    if(ExternalInterface.available) {
			   var getLevel : Function = function() : int {
			      return logger.MINlevel ;
			   }
			   ExternalInterface.addCallback("LoggerGetLevel", getLevel);
			   ExternalInterface.addCallback("LoggerChangeLevel", logger.changeLoggingLevel);
			   ExternalInterface.addCallback("LoggerIsStopped", logger.isStopped);
			   ExternalInterface.addCallback("LoggerStop", logger.stop);
			   ExternalInterface.addCallback("LoggerResume", logger.resume);	
               ExternalInterface.addCallback("LoggerReset", logger.reset);
               ExternalInterface.addCallback("LoggerIsInternalCopyKept", logger.isInternalCopyKept);	
               ExternalInterface.addCallback("LoggerEchoLogToInternalCopy", logger.echoLogToInternalCopy);	
               ExternalInterface.addCallback("LoggerClearBuffer", logger.clearBuffer);		
               ExternalInterface.addCallback("LoggerGetLog", logger.getResultAsString);					   
			   ExternalInterface.addCallback("getAbsState", logger.getAppAbsState);  
			   xtrace("FittestLogger's JavaScript APIs are set.") ;
		    }
			else xtrace("Fail to set FittestLogger's JavaScript APIs because ExternalInterface is not present...") ;
		}
		
		private function write_(s:String):void{
			if (keepInternalCopy) super.write(s) ;
			if (!fittestAgentCanAcceptLog) {
				tmpLogBuffer.writeUTFBytes(s); // by urueda
				return ;
			}
			_binarySocket.writeUTFBytes(s) ;
			_binarySocket.flush();
			_logActivity = true;
		}
		
		override protected function write(s:String):void{
			if (s == null || s.length == 0) { // by urueda
				return ;
			}
			write_(s) ;
		}
		
		override protected function writeBytes(s:ByteArray):void{
			if (s == null || s.length == 0) { // by urueda
				return ;
			}
			if (keepInternalCopy) super.writeBytes(s) ;
			if (!fittestAgentCanAcceptLog) {
				tmpLogBuffer.writeBytes(s,0,s.length); // by urueda
				return ;
			}
			_binarySocket.writeBytes(s) ;
			_binarySocket.flush();
			_logActivity = true;
		}
		
		override protected function writeLine(s : String) : void { 
			if (s == null || s.length == 0) { // by urueda
				return ;
			}
			write_(s + "\n") ;
		}
		
		/**
		 * Reset the state of the logger.
		 */
		public function reset() : void {
			clearBuffer() ;
		    tmpLogBuffer.clear(); // by urueda
			stop() ;
		}
		
		/**
		 * The xml-socket is used to exchange control messages with
		 * the FITTEST agent. This function will register a number
		 * of handlers for events on this socket. E.g. a handler for
         * incoming message, a handler when an IO-error is detected, etc.
		 */ 
		protected function registerHandlers(socket:XMLSocket):XMLSocket {
			socket.addEventListener(DataEvent.DATA, dataHandler);
			socket.addEventListener(Event.CONNECT, connectHandler);
			socket.addEventListener(Event.CLOSE, disconnectHandler); // by urueda
			socket.addEventListener(IOErrorEvent.IO_ERROR, ioHandler);
			socket.addEventListener(SecurityErrorEvent.SECURITY_ERROR, securityHandler);
			xtrace("Socket handlers are registered...") ;
			return socket;
		}
		
		/**
		 * Is called when connection to the FITTEST agent is established.
		 */ 
		protected function connectHandler(event:Event):void { 
			xtrace("Handling socket-connect: " + event) ;
			commandSocket = _currentSocket ;
			commandSocket.send("<Register><fittestEntityName>"
				+ getQualifiedClassName(this)
				+ "</fittestEntityName></Register>");
		}
		
		/**
		 * Is called when disconnected.
		 * by urueda
		 */ 
		protected function disconnectHandler(event:Event):void { // caution: if disconnected there will be no more logging!
			xtrace("Handling socket-disconnect: " + event) ;
			/*if (commandSocket != null) { // makes sense?
                commandSocket.send(createMessage("Deregister", _agentId, new Array("id", "towards"), new Array(_componentId, _iteId))); // by urueda
	        }*/
			if (_binarySocket != null) {
				_binarySocket.close();
			}
			_binarySocket = null;
		}
		
		/**
		 * Implementing the protocol with the FITTEST agent.
		 */ 
		protected function dataHandler(event:DataEvent):void {
			var data:XML = new XML(event.text);
			xtrace("Receiving data from FITTEST agent: " + data.name());
			if(data.name() == "RegisterResponse"){
				_componentId = data.fittestComponentId;
				_iteId = data.fittestIteId;
				_agentId = data.fittestAgentId;
				_componentDir = data.fittestComponentDir;
			}
			else if	(data.name() == "Initialize") {
                if (protocolState == STARTED|| protocolState == STOPPED) {
                  xtrace("FITTEST agent has commanded the logger to reset; but the logger is still running. Stop and terminate it first.") ;
                  return ;
                }
                
				xtrace("FITTEST agent has commanded the logger to reset...") ;
				reset() ; // reseting the logger ...
                // now getting the logging level:
                var level_ : int = 0 ;
                for each (var param:XML in data.parameter) {
                    if (param.@name == "loggingLevel") {
                       level_ = param.@value ;
                       break ;
                    } 
                } ;
                if (level_ > 0) {
                  MINlevel = level_ ;
                  xtrace("Setting logging level to " + MINlevel) ;
                }
                
				var now:Date = new Date();
				//_logFileName = "log_" + now.getTime() + ".log";
				_logTime = now.getTime(); // by urueda
				_logFileName = "log_" + _logTime + "_" + _logPeriod + ".log"; // by urueda
				commandSocket.send(createMessage(
							"StartSerialization",
							_agentId, 
							new Array("resource"), 
							new Array(_componentDir + _logFileName))
					);
                protocolState = INITIALIZED ;
			}
			else if (data.name() == "Start") {
				xtrace("FITTEST agent has commanded the logger to turn on logging...") ;
				resume() ;
				// _started = event;  does not seem to do anything useful
                protocolState = STARTED ;
			}
			else if (data.name() == "Stop") {
				xtrace("FITTEST agent has commanded the logger to stop logging...") ;
				stop() ;
                protocolState = STOPPED ;
			}			
			else if (data.name() == "Terminate") {
				xtrace("FITTEST agent has commanded the logger to terminate...") ;
				// check if we need to send pending data to _binarysSocket before terminating (i.e. tmpLogBuffer)
				if (tmpLogBuffer.length > 0) { // by urueda
					_binarySocket.writeBytes(tmpLogBuffer);
					tmpLogBuffer.clear();
				}				
				// no need to do anything on the logger itself...
				_binarySocket.close() ;
				fittestAgentCanAcceptLog = false ;
				commandSocket.send(createMessage("StopSerialization", _agentId, new Array("resource"), new Array(_componentDir + _logFileName)));
				commandSocket.send(createMessage("UploadResource",_agentId, new Array("resource", "towards"), new Array(_componentDir+_logFileName, _iteId)));
                protocolState = TERMINATED ;
                commandSocket.send(createMessage("Deregister", _agentId, new Array("id", "towards"), new Array(_componentId, _iteId))); // by urueda
			}
			else if (data.name() == "StartSerializationResponse") {
				_logActivity = false; // by urueda
				if (protocolState == UPLOADING && _binarySocket != null && _binarySocket.connected) { // by urueda
					if (tmpLogBuffer.length > 0) { // write any log data between log chunks
						_binarySocket.writeBytes(tmpLogBuffer);
						tmpLogBuffer.clear();
						//_logActivity = true;
					}
					fittestAgentCanAcceptLog = true;
				}
				else {
					var address: String = data.address;
					_binarySocket = new Socket();
					xtrace("A binary socket is created, on which we can write the log onto...") ;
					_binarySocket.addEventListener(Event.CONNECT, connected);//this is required: you can send data only when you are called back
					_binarySocket.connect(address.split(":")[0], address.split(":")[1]);
					//fittestAgentCanAcceptLog = true ; // commented by urueda
				}
			}
			else if (data.name() == "Upload" && (protocolState == STARTED || protocolState == UPLOADING) && fittestAgentCanAcceptLog) { // by urueda
				if (_logActivity) { // do not react to upload if there is no logging activity for the period
					fittestAgentCanAcceptLog = false ;
					xtrace("FITTEST agent has commanded the logger to upload...");
					protocolState = UPLOADING ;
					_binarySocket.writeUTFBytes("LOG_CHUNK_END"); // send the log chunk end
					_binarySocket.flush();
					commandSocket.send(createMessage("StopSerialization", _agentId, new Array("resource"), new Array(_componentDir + _logFileName)));
					setTimeout(handleUpload, 500, 1); // we delay the upload to happen after serialization has stopped
					setTimeout(handleUpload, 1000, 2); // we resume serialization and delay it to happen after uploading
				}
			}
		}
		
		// by urueda
		private function handleUpload(phase :uint):void {
			switch(phase) {
				case 1: // upload resource
					commandSocket.send(createMessage("UploadResource",_agentId, new Array("resource", "towards"), new Array(_componentDir+_logFileName, _iteId)));
					break;
				case 2: // resume serialization
					// increase the log chunk number				
					if (_logPeriod == int.MAX_VALUE) {
						_logPeriod = 0;
					}
					else {
						_logPeriod++;
					}
					// resume
					_logFileName = "log_" + _logTime + "_" + _logPeriod + ".log";
					commandSocket.send(createMessage("StartSerialization", _agentId, new Array("resource"), new Array(_componentDir + _logFileName)));
			}
		}
		
		/**
		 * This is called when connection to the FITTEST agent
		 * cannot be made. E.g. it can try to to connect on the
		 * next port.
		 */  
		public function ioHandler(event:IOErrorEvent):void {
			xtrace("IO error: " + event) ;
			tryNextPort() ;
		}
		
		private function tryNextPort() : void {
			_currentSocket = new XMLSocket();
			if (_currentPort < UNTIL) {			    
				_currentPort += BY;
				xtrace("Trying to connect to the Fittest agent at a new port: " + _currentPort) ;
				if(Security.sandboxType == Security.REMOTE){
					Security.loadPolicyFile("xmlsocket://localhost:" + _currentPort);
				}
				registerHandlers(_currentSocket).connect("localhost", _currentPort);
			}
		}
		
		/**
		 * This is called when connection to the FITTEST agent
		 * cannot be made due to a security problem. E.g. it can try 
		 * to to connect on the next port.
		 */ 
		public function securityHandler(event:SecurityErrorEvent):void {
			xtrace("Security error: " + event) ;
			tryNextPort() ;
		}
		
		/**
		 * To create a msg to be sent to the FITTEST agent.
		 */ 
		protected function createMessage(
			type:String, 
			to:String, 
			parameters:Array, 
			values:Array )
			: String 
		{
			var msg:String =  "<" + type + "><from>" + _componentId + "</from><to>" + to + "</to>";
			var i:int;  
			for (i = 0; i < (parameters.length); i++) {  
				msg += "<" + parameters[i] + ">" + values[i] + "</" + parameters[i] + ">"; 
			}  
			msg += "</" + type + ">";
			return msg ;
		}
		
		private function connected(event:Event):void {
			// the connection is maintained across uploads
			fittestAgentCanAcceptLog = true ; // by urueda			
			/* 
			this original logic does not seem to have any side effect (skip!), so
			I commented it out:
			
			if (_started != null) {//start has already been received
			dataHandler(_started);
			}
			*/		
		}
		
	}
}