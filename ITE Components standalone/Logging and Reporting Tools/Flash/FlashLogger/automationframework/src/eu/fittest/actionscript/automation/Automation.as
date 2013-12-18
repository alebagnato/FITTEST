/*

Authors: Arthur Baars, Wishnu Prasetya

Copyright 2011 Universitat Politècnica de València, Utrecht University

The use of this sofware is free under the Modified BSD License.

*/


package eu.fittest.actionscript.automation  
{
  import flash.display.Sprite;
  import flash.display.DisplayObject;
  import flash.display.DisplayObjectContainer;
  import flash.events.Event;
  import flash.events.EventDispatcher;
  import flash.external.ExternalInterface;
  import flash.utils.Dictionary;
  import flash.utils.getDefinitionByName;
  import flash.utils.getQualifiedClassName;
  import flash.utils.getQualifiedSuperclassName;
  import flash.utils.*;
  import flash.geom.*;
  
  import flash.events.UncaughtErrorEvent;
  import flash.events.ErrorEvent;

  //import eu.fittest.actionscript.automation.UnhandleException;
  
  /** 
    * Dispatched when an event is intercepted.
    *
    * @eventType eu.fittest.actionscript.automation.RecordEvent.RECORD
    * 
    * @see Automation#record()
    */
  [Event(name="record", type="eu.fittest.actionscript.automation.RecordEvent")]

  /** 
    * Dispatched after the Automation single global instance has been initialized.
    *
    * @eventType eu.fittest.actionscript.automation.InitEvent.INIT
    * 
    * @see Automation#init()
    */
  [Event(name="init", type="eu.fittest.actionscript.automation.InitEvent")]

  /**
    * This class allows user events to be intercepted, e.g. so that they can be logged.
    * It also allows events to be programmatically fired up. With
    * these two features we basically have the ability to record and replay events. User events
    * can be logged, and later replayed by interpreting the log, and execute the corresponding
    * events programmatically (rather than by manually clicking on objects on the screen).
    * 
    * <p>When an instance of this Automation class is "hooked" to the root DisplayObject of a target Flash 
    * application, it will scan this root for decendants DisplayObjects (GUI components). In addition,
    * the Automation is also aware when a DisplayObject is removed, or a new one is added. To each
    * DisplayObject o it finds, it will associate a delegate object, which is an instance of the class
    * Delegate. This delegate is the one that decides which events of o will be intercepted, and what 
    * to do with them. Additionally, this delegate also makes it possible for events of o to be replayed. </p>
    *
    * <p>When an event is intercepted, Automation does not change its semantic. When the corresponding
    * delegate decides to do something with it, it dispatches a RecordEvent, that simply contains a reference
    * to the intercepted event. RecordEvents are special events of the Automation. So far, when one is being 
    * fired up, it simply represents the fact that there was an event being intercepted. To actually do something with them,
    * e.g. to log them, we can simply define a handler for these events, and add it to Automation.</p>
    *
    * <p>Because people often want to be able to replay events in new executions, we need a way to find the
    * DisplayObject that is the target of the event, that is stable accross different runs. E.g. literally
    * using the pointer value of the object is not going to work, as it would change from one run to another.
    * The usual way to do it is by explicitly assigning unique name/ID string to each DisplayObject. A programmer
    * may already explicitly define this, but if it is missing someone has to construct it. Another role of
    * Delegates is to fetch this ID, or construct one if one is missing.</p>
    *
    * <p>Because different types of DisplayObjects may accept different types of events, each may need a matching
    * Delegate as well. To be able to determine which Delegate should be associated to a DisplayObject, 
    * Automation maintains a Dictionary containing pairs (D,L). Both are classes. Whenever a DisplayObject o is
    * added to the Application, Automation looks in this dictionary. Suppose o is of class D. If an entry
    * (D,L) can be found in the dictionary, then this L is the class of the Delegate that should be attached to o.
    * Else, Automation search for the first (D',L), where D' is a superclass of D. If no such mapping can be found,
    * then o is ignored. Thus, no events of o will be intercepted.</p>
    *
    * <p>The class is implemented the Singleton Pattern. This means that it has only
    * one (global) instance, which can be accessed as: Automation.instance. </p>
    * 
    * <p>We also exports some functions to JavaScript through the ExternalInterface, so that Automation can be
    * controlled externally from JavaScript. After calling the "init" method of the instance
    * the following methods will be registered to the ExternalInterface:</p>
    *
    * <ul>
    * <li><a href="Automation.html#startRecording()">StartRecording()</a>, to enable interception.</li>
    * <li><a href="Automation.html#stopRecording()">StopRecording()</a>, to disable interception.</li>
    * <li><a href="Automation.html#invoke()">Invoke()</a>, to programmatically fire up an event.</li>
    * <li><a href="Automation.html#testObject()">TestObject()</a>, to test if there exists a DisplayObject with the given id.</li>
    * </ul>
    * 
    * @see Delegate#replay()
    * @see Automation#invoke()
    * @see Automation#record()
    * @see Automation#init()
    * 
    * 
    */

  //change supper class on EventDispatcher insted of Sprite
  public class Automation extends Sprite
  {
    /**
      *  Holding a singleton instance of this class.
      */ 
    private static var INSTANCE:Automation;
    
    /**
      *  The delegate registry is a mapping from class names to their associated 
      *  delegate classes.
      */  
    private var delegateRegistry:Dictionary;

    /**
      *  Hold a mapping from object-IDs to their associated delegate-objects.
      *  
      *  <p>Change: now this contains only disp-objects whose IDs we beleive to be unique
      *  and stable. This implies that some instrumented disp-obj may not be in
      *  this map.</p>
      */ 
    private var objects:Dictionary;

    /**
      *  Hold a mapping from objects to their delegate-objects. Note that
      *  if no mathcing delegate-type can be found for a given disp-obj, then
      *  it will be mapped to a null (!); you need to take this into account
      *  when iterating over the map.
      */ 
    private var delegates:Dictionary;

    private var _recordMode:Boolean;

    // not used
    private var exception:String;
    
    /**
      *  Get the reference to the global, single Automation instance.
      */ 
    public static function  get instance():Automation
    {
      if(! INSTANCE)
      {
        INSTANCE = new Automation();
      }
      return INSTANCE;
    }

    public function Automation()
    {
      _recordMode = false;
      delegateRegistry = new Dictionary();
      objects = new Dictionary();
      delegates = new Dictionary();
    }


    /** 
      * Registers the component class and delegate class association with Automation.
      * @param target Class of UI components for which to register a delegate
      * @param delegate Class of the Delegate objects that are to be created and connected to 
      *   UI components of the target class 
      */
    public static function registerDelegateClass(target:Class,delegate:Class):void
    {
      registerDelegateClassByName(getQualifiedClassName(target),delegate);
    }

    /** 
      * Registers the component class and delegate class association with Automation.
      * @param target Name of the class of UI components for which to register a delegate
      * @param delegate Class of the Delegate objects that are to be created and connected to 
      *   UI components of the target class 
      */
    public static function registerDelegateClassByName(target:String,delegate:Class):void
    {
      instance.delegateRegistry[target] = delegate;
    }

    /**
      * Start recording Events. After calling this method RecordEvents will be dispatched for each detected user event.
      */ 
    public function startRecording():void
    {
      _recordMode = true;
    }
    
    /**
      * Stop recording Events. After calling this method no RecordEvent are dispatched.
      */ 
    public function stopRecording():void
    {
      _recordMode = false;
    }
    
    /**
      *  Flag expressing whether the Automation instance is currently in "recording mode"
      */ 
    public function get recordMode():Boolean
    {
      return _recordMode;
    }

    /**
      * Dispatch a RecordEvent, if the Automation is in the "recording mode". 
      * This is method is to be called by Delegates when they intercept an
      * event. 
      *
      * @param src The delegate object from which the event originates
      * @param cmd The Command object representing the event
      * 
      */
    public static function record(src:Delegate, cmd:Command):void
    {
      if(instance._recordMode)  {
         var o:DisplayObject = src.target ;
         // trace("## del.id=" + src.id + ", o.id=" + o["id"] + ", o.automationName=" + o["automationName"]) ;
         if (src.id.substr(0,3)=="IDY") { 
            // The delegate has aquired a unique-name assigned by the programmer;
            // this is fine.
         }
         else {
            // check first if an automationName is available on the disp-obj:
            try { 
               var autName : String  = o["automationName"] ;
               if (autName && (autName.substr(0,3)=="IDY" || autName.length > src.id.length)) {
               // the object has acquired a programmatically assigned unique name,
               // or a name that seems to be more specific that the current used id
               var oldId : String = src.id ;
               src.id = autName ;
               delete instance.objects[oldId] ;
               instance.objects[src.id] = src ;
               }
            }               
            catch (e : Error) { 
               // fail.. probably because the disp-obj has no automationName property;
               // then we don't do anything with it either
            }
         }
         instance.dispatchEvent(new RecordEvent(src, cmd));
      }
    }

    /**
     * Return matching IDs in the objects-map.
     */
    public function getAutomationId(prefix:String):Array
    {
      var rb:Array = new Array();
      for (var key:String in objects) {
        if (key.substr(0, prefix.length) == prefix) rb.push(key) ;
      }
      return rb;
    }

    /**
     * Return the ID of the object associated to the Delegate with the given ID.
     */
    public function getObjectNameByDelegateId(id:String):String {
       for each (var d in delegates) {
           if (d == null) continue ;
           var del : Delegate = d as Delegate ;
           if (del.id == id) {
              try { return del.target["automationName"] ; }
              catch (e : Error) { }
           }
       } 
       return "";
    }

    /**
     * Return the ID of the Delegate associated to the DisplayObject with the given ID.
     */
    public function getDelegateIdByObjectName(name:String):String {
      for each (var d in delegates) {
           if (d == null) continue ;
           var del : Delegate = d as Delegate ;
           try { 
              if (del.target["automationName"] == name) return del.id ;
           }
           catch (e : Error) { }
       } 
       return "";
    }
  
    // change the delegate-id with its disp-obj name(!);
    // provided for compatibility with Alexander's modules; 
    // "name" can however change at the runtime, so note that
    // applying this may result in a log entry that cannot be replayed.
    //
    public function changeDelegateIdByName(name:String):Boolean {
        for each (var d in delegates) {
           if (d == null) continue ;
           var del : Delegate = d as Delegate ;
           if (del.id == name) {
              try {
                 var name2 : String = del.target["name"] ;
                 del.id = name2 ;
                 delete objects[name] ;
                 objects[name2] = del.target ;
                 return true ;
              }
              catch(e : Error) { return false ; }
           }
        }
        return false ;        
    }
    
    
//     private function applicationCompleteHandler(event:Event): void 
//     {
//       loaderInfo.uncaughtErrorEvents.addEventListener(UncaughtErrorEvent.UNCAUGHT_ERROR, uncaughtErrorHandler);
//     }

//     private function uncaughtErrorHandler(e:UncaughtErrorEvent):void 
//     {
//       e.preventDefault();
//       var s:String;
//       if (e.error is Error)
//       {
//         var error:Error = e.error as Error;
//         s = "Uncaught Error: " + error.errorID + ", " + error.name + ", " + error.message;
//       }
//       else
//       {
//         var errorEvent:ErrorEvent = e.error as ErrorEvent;
//         s = "Uncaught ErrorEvent: " + errorEvent.errorID;
//       }
//       trace(s);
//       instance.exception = s;
//     }

    // not used
    public function getUException():String
    {
      return exception;
    }

    /**
      * Initialize the Automation instance, connecting it to a UI component. Dispatches an event of type InitEvent.INIT containing the root DisplayObject.conver
      * @param root The root of the tree of UI components that are to be monitored 
      * 
      * @see eu.fittest.actionscript.automation.InitEvent
      */ 
    public static function init(root:DisplayObject):void
    {
      trace("** Automation: entering init ...") ;
      //trace("** check root instance: " + (root is Application));
      instance.connectDelegate(root);
      instance.scan(root);
      // add methods to ExternalInterface and setup ExternalInterface to marshal Exceptions to the caller (Javascript)

      //var genericExHdl:UnhandleException = new UnhandleException(root);
      if(ExternalInterface.available)
      {
        trace("** Automation: external interface is available.");
        ExternalInterface.addCallback("Invoke", instance.invoke);
        //trace("** Invoke externalized...") ;
        ExternalInterface.addCallback("InvokeAndCheck", instance.invokeAndCheck);
        //trace("** InvokeAndCheck externalized...") ;
        ExternalInterface.addCallback("TestObject", instance.testObject);
        ExternalInterface.addCallback("StartRecording", instance.startRecording);
        ExternalInterface.addCallback("StopRecording", instance.stopRecording);
        ExternalInterface.addCallback("getAutomationId", instance.getAutomationId);
        ExternalInterface.addCallback("getObjectNameByDelegateId", instance.getObjectNameByDelegateId);
        ExternalInterface.addCallback("getDelegateIdByObjectName", instance.getDelegateIdByObjectName);
        ExternalInterface.addCallback("changeDelegateIdByName", instance.changeDelegateIdByName);
        //ExternalInterface.addCallback("getUException", genericExHdl.getUException);
	    //ExternalInterface.addCallback("getUException", instance.getUException);
        // to make exception thrown by Automation's external interfaces to be marshalled to string, 
        // rather than popping up a blocking alert box:
        ExternalInterface.marshallExceptions = true;    
        //trace("evidence of changes: " + (new Date()));
        trace("** Automation: done with externalizing its functions.") ;
      }
      // add listeners for addition and removal of components to the display
      root.addEventListener(Event.REMOVED,instance.removedFromDisplayList, false, -10);
      root.addEventListener(Event.ADDED,instance.addedToDisplayList, false, -10); 

      // root.addEventListener("applicationComplete", genericExHdl.applicationCompleteHandler)
//       var applicationCompleteHandler:Function = function(event:Event): void 
//             {
// 	      trace("** applicationCompleteHandler has fired!!");
//               root.loaderInfo.uncaughtErrorEvents.addEventListener(UncaughtErrorEvent.UNCAUGHT_ERROR, instance.uncaughtErrorHandler);
//             }
      //root.addEventListener("applicationComplete", applicationCompleteHandler);
//       trace("**before: " + root.willTrigger(UncaughtErrorEvent.UNCAUGHT_ERROR));
//       root.loaderInfo.uncaughtErrorEvents.addEventListener(UncaughtErrorEvent.UNCAUGHT_ERROR, instance.uncaughtErrorHandler);
//       trace("**after: " + root.willTrigger(UncaughtErrorEvent.UNCAUGHT_ERROR));
      instance.dispatchEvent(new InitEvent(root));
    }
    
		public static function sleep(ms:int):void {
		  var init:int = getTimer();
		  while(true) {
		    if(getTimer() - init >= ms) {
		      break;
		    }
		  }
		}
				

		/**
		  * For a given object this function returns true if this object including 
		  * all its ancestors until the root are visible, i.e. obj.visible = true, 
		  * otherwise it returns false.But it dosn't give us a criterion to decide 
		  * whether the object is visible on the screan and the user can dispach an 
		  * event with that object as the target.As result it can now be safely removed.
		  */  
		function isVisibleOnDisplayList(dobj:DisplayObject):Boolean {
		  if (dobj) {
		    if (!dobj.visible) {
		      return false;
		    } else {
			return isVisibleOnDisplayList(dobj.parent);
		    }
		  } 
		  return true;
		}

		/**
		  A helper function taken from the adobe documentation on the topic 
		  how traverse display list object.
		**/
		function traceDisplayList(container:DisplayObjectContainer, indentString:String = ""):void { 
		  var child:DisplayObject;	 
		  for (var i:uint=0; i < container.numChildren; i++) { 
		    child = container.getChildAt(i); 
		    trace(indentString, child, child.name, child.visible);  
		    if (container.getChildAt(i) is DisplayObjectContainer) { 
		      traceDisplayList(DisplayObjectContainer(child), indentString + "    ") 
		    } 
		  } 
		}
        
        /** test wether an object with given objectID exists according to Automation.
		 * @param objectID The identifier of an object
		 * @return True when the objectID is found, False otherwise
		 */
		public function testObject(objectID:String):Boolean
		{
		  return findObject(objectID);
		  //return isVisibleOnDisplayList(findObject(objectID));
		}

        // check if an object with this ID can potentially be found by Automation.
        private function findObject(objectID:String):DisplayObject {
          for each (var d in delegates) {
             // check first for matching automation-name
             if (d == null) continue ;
             var del : Delegate = d as Delegate ;
             var autName : String ;
             try { autName = del.target["automationName"] } catch (e : Error) { continue ; }
             if (autName == objectID) return del.target ;
          }
          for each (var d_ in delegates) {
             // then matching delegate-id
             if (d_ == null) continue ;              
             var del : Delegate = d_ as Delegate ;
             if (del.id == objectID) return del.target ;
          }  
          trace("object with id" + objectID + " doesn't exist!");
          return null;
        }
		
        
        public function invokeAndCheck(objectID:String, action:String, ... args) : String {
             try {
                if (args==null || args.length == 0) invoke(objectID,action) ;
                else invoke(objectID,action,args) ;
             }
             catch(e : Error) {
               return ("ERROR: " + e) ;
             }
             return "OK" ;
        }
        
		/** 
         * Replay a command on the Delegate object corresponding to objectID passing the provided action and arguments.
		 * @param objectID The identifier of the Delegate object on which to invoke the command
		 * @param action The name of the Command's action  
		 * @param ...args The argument list for the invoked Command
		 * @return The result returned by the Command being invoked
		 * 
		 * @throws Error when the objectID is not found
		 * 
		 * @see Command
		 * @see Delegate#replay()
		 */  
		public function invoke(objectID:String, action:String, ... args):Object
		{
			// trace("## invoke on " + objectID + ", event " + action);
            // look in the objects-table first:
			var delegate:Delegate = objects[objectID];
			if(delegate) {
				return delegate.replay(new Command(action,args));
			}
			else {
              // The ID cannot be found. Then it gets complicated. We first check
              // if there is a matching automationName:
              // trace("## --1 about to iterate over all delegates...") ;
              for each (var d_ in delegates) {
                 if (d_ == null) continue ;    
                 var del : Delegate = d_ as Delegate ;                 
                 var autName : String ;
                 try { autName = del.target["automationName"] } catch (e : Error) { continue ; }
                 // trace("## --2b " + autName + " " + del["id"]  ) ;
                 if (autName == objectID) {
                    // the searched ID matches the display-obj automationName! 
                    // we will also change the used delegate-id to this automationName,
                    // and add the link to the objects-table:
                    var oldId : String = del.id ;
                    del.id = objectID ;
                    delete objects[oldId] ;
                    objects[objectID] = del ;
                    // trace("## --3") ;
                    return del.replay(new Command(action,args));
                 }
              }
              // ok.. so no matching automationName was found. As last
              // resort to try to find a matching delegate-id, knowing that these may
              // not be unique nor stable under future GUI changes in the 
              // target application:  
              // trace("## --4 about to run 2nd iteration over all delegates...") ;
              for each (var d_ in delegates) {
                 if (d_ == null) continue ;              
                 // trace("## --5a") ;
                 var del : Delegate = d_ as Delegate ;
                 // trace("## --5b, type of del: " + getQualifiedClassName(del)) ;
                 if (del.id == objectID) {
                    // trace("## --6") ;
                    // not safe .. but well, the only thing left we have:
                    return del.replay(new Command(action,args));
                 }
              }         
              // no matching id can be found:              
              throw new Error("Object with this ID not found: " + objectID );
            }
		}

		
		private function removedFromDisplayList(ev:Event):Boolean
		{
			return removeDisplayObject(ev.target);
		}
		
		// remove a component and all its children from the dictionaries
		private function removeDisplayObject(obj:Object):Boolean
		{
			if(obj in delegates)
			{
				disconnectDelegate(obj);
				if(obj is DisplayObjectContainer)
				{
					for (var i: int; i < obj.numChildren; i++)
					{
						removeDisplayObject(obj.getChildAt(i));
					}
				}				
			}
			return true;
		}
		
		private function addedToDisplayList(ev:Event):Boolean
		{
			//trace("**Automation senses a display object being added...") ;
			return addDisplayObject(ev.target);
		}
		
		private function addDisplayObject(obj:Object):Boolean
		{
			if(obj is DisplayObject)
			{
				var x:DisplayObject = obj as DisplayObject;
				while(!(x in this.delegates))
				{
					x = x.parent;
				}
				scan(x);
				return true;
			}
			else
				return false;
		}

		private function scan(obj:DisplayObject):void
		{
			  //var automateChildren:Boolean = delegates[obj] == null || (delegates[obj] as Delegate).automateChildren;
			 var automateChildren:Boolean = true;
			if(automateChildren && obj is DisplayObjectContainer)
			{
				var parent:DisplayObjectContainer = (obj as DisplayObjectContainer);
				for(var i:int; i < parent.numChildren; i++)
				{
					var child:DisplayObject = parent.getChildAt(i);
					if(! (child in delegates))
					{
						connectDelegate(child);
						scan(child);
					}
				}					
			}
		}
		
		private function connectDelegate(obj:DisplayObject):void
		{
			// check if the display-obj is in the ignore-list:
			if (dontConnectThis(obj)) {
				delegates[obj] = null ;
				return ;
			}
			
			var delegate:Delegate = null;
			
			var delegateClass:Class = findDelegateClass(obj);
			if(delegateClass)
			{
				delegate = new delegateClass(obj) ;
                // Only add entry to the objects-table if we are sure the id
                // is unique:
				// objects[delegate.id] = delegate;
                if (delegate.id.substr(0,3) == "IDY") objects[delegate.id] = delegate ;
			}
			// store delegate for component obj
			delegates[obj] = delegate;
			if(delegateClass)
			{
			    //trace("## an object of class " + getQualifiedClassName(obj) + " ---> id:" +delegate.id)
			}
		}
		
		// implementing the ignore-list. Work this out as needed...
		private function dontConnectThis(obj:DisplayObject) : Boolean {
			try {
				var id_ : String = (obj["id"]) as String ;
				if (id_.substr(0,3) == "___")
				return true ;
			}
			catch (e:Error) { return false; }	
			return false ;
		}
		
		private function disconnectDelegate(obj:Object):void
		{
			var delegate:Object = delegates[obj];
			if(delegate )
			{
				delete objects[delegate.id];
			}
			delete delegates[obj];
		}

		private function findDelegateClass(obj:Object):Class		
		{
			var className:String = getQualifiedClassName(obj);
			//trace("className --> " + className);
			if(className == null || className == "Object")
			{
				return null;
			}	
			else if(className in delegateRegistry)
			{
				return delegateRegistry[className];
			}
			else
			{
				var superClass:String = getQualifiedSuperclassName(obj);
				var clazz:Object = getDefinitionByName(superClass);
				return findDelegateClass(clazz);
			}
		}
	}
}