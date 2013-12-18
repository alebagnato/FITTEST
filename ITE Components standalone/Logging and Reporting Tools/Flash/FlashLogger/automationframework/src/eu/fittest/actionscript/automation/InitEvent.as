package eu.fittest.actionscript.automation
{
	import flash.display.DisplayObject;
	import flash.events.Event;
	
	/**
	 * An InitEvent is dispatched by the Automation instance after its init method has been invoked.
	 * @see eu.fittest.actionscript.automation.Automation#init() 
	 * 
	 */ 
	public class InitEvent extends Event
	{
		/**
		 * @eventType "eu.fittest.Automation.INIT"
		 */ 
		public static const INIT : String = "eu.fittest.Automation.INIT";
		
		private var _root:DisplayObject;
		
		/**
		 *
		 * @param root The root DisplayObject to which the Automation instance is connected
		 */  
		public function InitEvent(root:DisplayObject)
		{
			super(INIT);
			this._root = root;
		}
		/**
		 * The root DisplayObject
		 */ 
		public function get root():DisplayObject
		{
			return _root;
		}
	}
}