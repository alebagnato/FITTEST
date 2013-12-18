package eu.fittest.actionscript.automation.loader 
{
	import eu.fittest.actionscript.automation.Automation;
	import eu.fittest.actionscript.automation.RecordEvent;
	
	import flash.display.DisplayObject;
	import flash.display.Loader;
	import flash.display.LoaderInfo;
	import flash.display.MovieClip;
	import flash.display.StageAlign;
	import flash.display.StageScaleMode;
	import flash.events.Event;
	import flash.external.ExternalInterface;
	import flash.net.URLRequest;
	import flash.system.ApplicationDomain;
	import flash.system.LoaderContext;
	import flash.utils.*;

	import flash.events.UncaughtErrorEvent;
	import flash.events.ErrorEvent;


	/**
	 * The AutomationLoader is a Flash application to dynamically load another Flash 
     * application and extra libraries in SWF format. 
	 * The loader will attach an instance of the Automation class to the loaded
     * application. This instance of Automation intercepts user events on the loaded application,
     * e.g. so that we can log them.
	 * Typically the extra libraries contain implementations of Delegate classes
     * (to handle the interception of events targetting specified
     * classes of GUI components), and/or Logging code.
	 * 
	 * <p>To load the other application and extra libraries the method 
     * loadApplication can be used, both directly or through the JavaScript. </p>
	 *  
     * <p>For debugging the class also exports the function GetUException to
     * JavaScript. It will return information about the last exception thrown by
     * the loaded application, if any.</p>
     *
	 * @see eu.fittest.actionscript.automation.Automation
	 * @see eu.fittest.actionscript.automation.Delegate
	 * @see #loadApplication()
	 */ 
	public class AutomationLoader extends MovieClip
	{
		private var index:int;
		private var libURLs : Array;
		private var application:DisplayObject;
		private var initCalled:Boolean;

		private var exception:String = "";

		/**
		 * Create AutomationLoader.
		 */ 
		public function AutomationLoader()
		{ 
			initCalled = false;
			if(stage)
			{
				setupStage();
			}
			else
			{
				addEventListener(Event.ADDED_TO_STAGE, setupStage);
			}
			if(ExternalInterface.available)
			{
				ExternalInterface.addCallback("loadApplication",loadApplication);
                ExternalInterface.addCallback("GetUException", getUException);
				ExternalInterface.addCallback("SetUException", setUExceptioon);
			}
		}
		

		private function getUException():String
		{
		    return exception;
		}

		private function setUExceptioon(s:String):void
		{
		    exception = s;
		}

		/**
		 * Load an application and extra libraries.
		 * @param appURL URL referring to the SWF file of a Flash application
		 * @param ...libURLs URLs referring to zero or more SWF files containing extra libraries to be loaded 
		 */ 
		public function loadApplication(appURL:String, ... libURLs ):void
		{	
    
			//trace("call to loadApplication")  
			if(!initCalled)
			{
				initCalled = true;
				index = 0;
				this.libURLs = libURLs;
				loadSWF(appURL, applicationLoaded);
			}
		}

		private function setupStage(ev:Event = null):void
		{
			stage.scaleMode = StageScaleMode.NO_SCALE;
			stage.align = StageAlign.TOP_LEFT;		
						
			if(ev!= null)
				removeEventListener(Event.ADDED_TO_STAGE,setupStage);
		}
		
		private function loadSWF(swfURL:String, handle:Function):void
		{
			var url:URLRequest = new URLRequest(swfURL); 
			
			var myLoader:Loader = new Loader();
			var context:LoaderContext = new LoaderContext(false,ApplicationDomain.currentDomain, null);
			myLoader.contentLoaderInfo.addEventListener(Event.COMPLETE,handle);
			myLoader.load(url, context);
			myLoader.uncaughtErrorEvents.addEventListener(UncaughtErrorEvent.UNCAUGHT_ERROR, uncaughtErrorHandler);
		}
		
		private function uncaughtErrorHandler(e:UncaughtErrorEvent):void
		{
		    e.preventDefault();
		    var s:String;
		    if (e.error is Error)
		    {
			var error:Error = e.error as Error;
			s = "Uncaught Error: " + error.errorID + ", " + error.name + ", " + error.message + error.getStackTrace();
		    }
		    else if (e.error is ErrorEvent)
		    {
			var errorEvent:ErrorEvent = e.error as ErrorEvent;
			s = "Uncaught ErrorEvent: " + errorEvent.errorID + errorEvent.text;
		    }
		    else
		    {
			s = "Unknown Error: " + e.error.toString();
		    }
		    trace(s);
		    exception = s;
		}

		private function libraryLoaded(ev:Event):void
		{
			var lib:DisplayObject = (ev.target as LoaderInfo).content as DisplayObject;
			loadLibs();
		}
		private function applicationLoaded(ev:Event):void
		{
			application = (ev.target as LoaderInfo).content as DisplayObject;

			addChild(application);
			loadLibs();
		}
		private function loadLibs(ev:Event = null):void
		{
			if(index >= libURLs.length)
			{
				//sleep(5000);
				Automation.init(application);  
                // adding the Automation-instance to this movie-clip, so that uncaught exception
                // it throws will be detected by the movie-clip's loader:
                // Well... it does not work; exception from Invoke is not registered :(
                // this.addChild(Automation.instance) ;
                // Automation.instance.root.loaderInfo.uncaughtErrorEvents.addEventListener(UncaughtErrorEvent.UNCAUGHT_ERROR, uncaughtErrorHandler);
			}
			else
			{
				var libURL:String = libURLs[index];
				index++;
				loadSWF(libURL,libraryLoaded);
			}
		}
				    
		private function sleep(ms:int):void {
		  var init:int = getTimer();
		  while(true) {
		    if(getTimer() - init >= ms) {
		      break;
		    }
		  }
		}
	}	
}