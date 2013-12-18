package eu.fittest.actionscript.automation.delegates.flex
{	
	import mx.controls.ButtonBar;
	import mx.events.ItemClickEvent;
	import eu.fittest.actionscript.automation.Automation;
	import eu.fittest.actionscript.automation.Command;
	import eu.fittest.actionscript.automation.Delegate;

	/**
	 * Implementation of a Delegate for mx.controls.ButtonBar components
	 * It handles capture and replay of ItemClickEvent.ITEM_CLICK events.
	 * 
	 * @see mx.controls.ButtonBar
	 * @see mx.events.ItemClickEvent
	 */ 
	public class ButtonBarDelegate extends Delegate
	{
		private static var counter:int = 0;

		/**
		 * Create a ButtonBarDelegate, which will listen for events of type ItemClickEvent.ITEM_CLICK on the
		 * given ButtonBar.
		 * @param bar A ButtonBar
		 */ 
		public function ButtonBarDelegate(bar:ButtonBar)
		{
			super(bar);
			bar.addEventListener(ItemClickEvent.ITEM_CLICK,clickItemHandler, false, -10);
		}
		
		private function clickItemHandler(ev:ItemClickEvent):void
		{
			Automation.record(this, Command.create("itemclick", ev.index));
		}
		

		
		protected override function automationId():String
		{
			return "ButtonBar"+counter++;
		}
		/**
		 * Dispatch an event of type ItemClickEvent.ITEM_CLICK on the connected ButtonBar.
		 * @param index Index of the item to be clicked
		 */ 
		public function itemclick( index:int):void
		{
			(object as ButtonBar).selectedIndex = index;
			// this event supposed not to bubble:
            object.dispatchEvent(new ItemClickEvent(ItemClickEvent.ITEM_CLICK, false,false,null,index));
		}
	}
}