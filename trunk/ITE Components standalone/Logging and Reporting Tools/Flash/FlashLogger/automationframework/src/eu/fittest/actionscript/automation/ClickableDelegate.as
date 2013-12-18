package eu.fittest.actionscript.automation
{
	import flash.display.DisplayObject;
	import flash.events.Event;
	import flash.events.MouseEvent;
		
	/**
	 * The class ClickableDelegate implements a generic class for clickable objects.
	 * It handles capture and replay of MouseEvent.CLICK events.
	 * @see flash.events.MouseEvent
	 */ 
		
	public class ClickableDelegate extends Delegate
	{
			
		/**
		 * Create a ClickableDelegate, which will listen for events of type 
         * MouseEvent.CLICK on the given DisplayObject. This is set by
         * setting a handler for the event. 
         *
		 * @param object A DisplayObject that is clickable (i.e. dispatches MouseEvent.CLICK)
		 */ 
		public function ClickableDelegate(object:DisplayObject)
		{
			super(object);
            // Set a handler, at bubble-phase with low priority.
			object.addEventListener(MouseEvent.CLICK,clickHandler, false, -10);
            //trace("** Automation registedred a handler with priority -10") ;
		}	

		private function clickHandler(ev:Event):void
		{
			Automation.record(this, Command.create("click"));
            //trace("** Automation's handler of click executed");  
		}
				
		/**
		 * Replay a click event on the connected DisplayObject by dispatching an event of type
		 * MouseEvent.CLICK
		 * 
		 */ 
		public function click():void
		{
			object.dispatchEvent(new MouseEvent(MouseEvent.CLICK, true,false));
		}
	}
}