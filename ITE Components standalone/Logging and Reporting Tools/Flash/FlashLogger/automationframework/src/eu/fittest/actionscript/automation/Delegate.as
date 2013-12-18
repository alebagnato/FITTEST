package eu.fittest.actionscript.automation
{
	import flash.display.DisplayObject;
	import flash.utils.getQualifiedClassName;
	
	/**
	 * A Delegate object defines the interface between the Automation instance and a DisplayObject. 
	 * A Delegate object is associated with a particular DisplayObject and will listen for (some of) its Events.
	 * Whenever the Delegate object detects an Event of the associated DisplayObject it notifies the Automation instance by passing the 
	 * Event's information in the form of a Command object to the method Automation.record(). 
	 * 
	 * Furthermore a Delegate object has a method replay that is able to replay a given Command. 
	 * 
	 * An implementation of the Delegate class must ensure that every Command it records is also replayable.
	 * 
	 * @see #replay()
	 * @see eu.fittest.actionscript.automation.Automation#record
	 * @see eu.fittest.actionscript.automation.Command
	 * 
	 */ 
	public class Delegate
	{
		/**
		 *  DisplayObject associated with this Delegate
		 */  
		protected var object:DisplayObject;
		/**
		 * Flag denoting whether the child objects of the associated DisplayObject must be automated as well. When this flag is False, the children of the associated 
		 * DisplayObject will be ignored by the Automation instance.
		 *  
		 * By default this property is set to False, sub-classes may change this behaviour
		 */ 
		protected var _automateChildren:Boolean;
		private var _id:String;

		/**
		 * @param object DisplayObject associated with this Delegate
		 * 
		 * By default automateChildren is set to False
		 * @see #_automateChildren
		 * 
		 */ 
		public function Delegate(object:DisplayObject)
		{
			this._automateChildren = false;
			this.object = object;
			this._id = automationId();
            //trace("## creating a delegate with id=" + id + ", o.id=" + object["id"] + ", o.name=" + object["name"]) ;
		}			
        
        public function get target() : DisplayObject { return object ; }

		/**
		 * Compute an identifier for the associated DisplayObject. This identifier should be unique, 
		 * stable between different executions, and preferrably human-readable.
		 * @return The automation identifier for the associated DisplayObject
		 * 
		 * The is an abstract method and needs to be redefined by a sub-class
		 */ 
		protected  function automationId():String
		{
			throw new Error("The method Delegate.automationId() is abstract and must be implemented by: " + getQualifiedClassName(this));
		}
		
		/**
		 * Flag denoting whether the child objects of the associated DisplayObject must be automated as well. When this flag is False, the children of the associated 
		 * DisplayObject will be ignored by the Automation instance.
		 */ 
		public function get automateChildren():Boolean
		{
			return this._automateChildren;
		}
		
		/**
		 *  The object identifier for the associated DisplayObject
		 */
		public function get id():String
		{
			return this._id;
		}

		public function set id(new_id:String):void
		{
		    this._id = new_id;
		}
		
        
        // a common helper function to construct a hierarchical-ID based on
        // position in the children-list of the containing disp-obj.
        public function constructHierarchicalName(o : DisplayObject) : String {
           try {
              if (o["automationName"]) return "HID_" + o["automationName"] ;
           }
           catch (e : Error) { }
           if (o.parent == null) return null ;
           var N : String = constructHierarchicalName(o.parent) ;
           if (N==null) return N ;
           return N + "_" + o.parent.getChildIndex(o) ;
        }
        
        
		/**
		 * Replay a Command
		 * 
		 * @param Command to be replayed
		 * @return The result returned by replaying the action specified by the Command, if any
		 * 
		 * The default implementation of this method interprets the Command's action as a method name. It will
		 * invoke the corresponding method of this Delegate passing it the arguments specified by the Command object.
		 * 
		 * @throw Error When the Command's action name can not be not interpreted by this delegate 
		 */ 
		public function replay(cmd:Command):Object
		{
			if(cmd.action in this)
			{
				var func:Object = this[cmd.action];
				if (func is Function)
				{	
					return (func as Function).apply(this, cmd.args);						
				}
				else
				{
					throw new Error("Property " + cmd.action + " exists, but is not a method: " + id + " of class " + getQualifiedClassName(this));
				}
			}
			else
				throw new Error("Method " + cmd.action + " not found for: " + id + " of class " + getQualifiedClassName(this));

		}
	}
}