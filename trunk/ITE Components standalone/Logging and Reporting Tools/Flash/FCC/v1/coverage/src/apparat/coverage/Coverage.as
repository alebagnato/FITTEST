package apparat.coverage
{
	import flash.events.*;
	import flash.net.*;
	import flash.system.Security;
	import flash.utils.Dictionary;
	
/**
  * Enable the coverage Class to communicate covered source file lines through a socket.
  * The socket IP and port are configured as established in a text file "coverage_iport.txt". 
  * Universitat Politecnica de Valencia 2013
  * Camino de Vera, s/n
  * 46022 Valencia, Spain
  * www.upv.es
  */

/**
  * author: Urko Rueda Molina
  * version 1.0
  * package coveragecollector
  */	
  
	public class Coverage
	{
		private static var socket:Socket  ;
		private static var connecting:Boolean = false;		

		private static var map:Dictionary = new Dictionary();
		private static var buffer:Array = new Array();
		
		private static function establishConnection():void
		{	
			if (!connecting)
			{
				connecting = true;
				// read address&port to connect to the coverage tool
				var iport_url:URLRequest = new URLRequest("coverage_iport.txt");
				var loader:URLLoader = new URLLoader();
				loader.load(iport_url);
				// wait file loading
				loader.addEventListener(Event.COMPLETE, loaderComplete);
				function loaderComplete(e:Event):void
				{
					connect(loader.data);
				}			
			}
		}
		
		private static function connect(iport:Object):void
		{			
			var pattern:RegExp = /<iport>[0-9]+.[0-9]+.[0-9]+.[0-9]+:[0-9]+<\/iport>/i;
			var matches:Object = iport.match(pattern);
			if (matches)
			{
				iport = iport.split("<iport>").join("");
				iport = iport.split("</iport>").join("");
				var tokens:Array = iport.split(":");
				var address:String = tokens[0];
				var port:int = tokens[1];
				createSocket(address,port);
			}
			else
			{
				trace("ip&port expected but got: " + iport);
			}		
		}	
		
		private static function createSocket(address:String, port:int):void
		{
			if (socket == null)
			{
				trace("Going to connect to address: " + address + " & port: " + port);
				//Security.allowDomain("*");
				//Security.loadPolicyFile("xmlsocket://localhost:12345");
				Security.loadPolicyFile("xmlsocket://" + address + ":" + port);

				socket = new Socket();
			
				socket.addEventListener(Event.CONNECT, onConnect);
				socket.addEventListener(ProgressEvent.SOCKET_DATA, onResponse);
				socket.addEventListener(Event.CLOSE, onClose);
				socket.addEventListener(IOErrorEvent.IO_ERROR, onError);
				socket.addEventListener(SecurityErrorEvent.SECURITY_ERROR, onSecError);
			
				socket.connect(address,port); //"localhost", 12345);		
			}
		}		
		
		private static function onResponse(event:ProgressEvent):void
		{			
			if (socket.connected)
			{
				var msg:String = "";
				while(socket.bytesAvailable)
				{
					msg = msg + socket.readUTFBytes(socket.bytesAvailable);
				}
				trace("Received: " + msg);
			}
		}		
		
		public static function onSample( file:String, line:int):void
		{
			trace("File line: " +file + " (" + line + ")");

			var item:String = file + ":" + line;
			if(! (item in map))
			{
				buffer.push(item);
				map[item] = item;
			}
			
			if (socket == null)
			{
				establishConnection();
			}
			
			if(socket != null && socket.connected && buffer.length > 0)
			{
				flush();
			}
		}
		
		/*
		private static function getClassName(file:String):String
		{
			var parts:Array = file.split(";");
			var packageName:String = parts[1];
			var className:String = parts[2];
			return parts[1] + (packageName.length == 0 ? "" : "/") + parts[2];
		}*/
		private static function onConnect(e:Event):void {
			flush();		
			
		}
		private static function flush():void 
		{
			while(buffer.length > 0)
			{ 
				socket.writeUTFBytes(buffer.shift());
				socket.writeUTFBytes("\n");
			}
			socket.flush();
		}
		
		private static function onClose(e:Event):void {
			// Security error is thrown if this line is excluded
			socket.close();
		}
		
		private static function onError(e:IOErrorEvent):void {
			trace("IO Error: "+e);
		}
		
		private static function onSecError(e:SecurityErrorEvent):void {
			trace("Security Error: "+e);
		}
		
	}
}
