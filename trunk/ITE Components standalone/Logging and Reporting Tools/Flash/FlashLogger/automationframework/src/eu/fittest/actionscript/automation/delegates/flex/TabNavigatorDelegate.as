package eu.fittest.actionscript.automation.delegates.flex
{	
	import mx.containers.TabNavigator;
	import mx.events.IndexChangedEvent;
	import eu.fittest.actionscript.automation.Automation;
	import eu.fittest.actionscript.automation.Command;
	import eu.fittest.actionscript.automation.Delegate;

	/**
	 * Implementation of a Delegate for mx.controls.TabNavigator components
	 * It handles capture and replay of IndexChangedEvent.CHANGE events.
	 * 
	 * @see mx.controls.TabNavigator
	 * @see mx.events.IndexChangedEvent
	 */ 
	public class TabNavigatorDelegate extends Delegate
	{
		private static var counter:int = 0;

		/**
		 * Create a TabNavigatorDelegate, which will listen for events of type IndexChangedEvent.CHANGE on the
		 * given TabNavigator.
		 * @param bar A TabNavigator
		 */ 
		public function TabNavigatorDelegate(tab: TabNavigator)
		{
			super(tab);
			_automateChildren = true;
			tab.addEventListener(IndexChangedEvent.CHANGE, changeHandler, false, -10);
		}
		
		private function changeHandler(ev:IndexChangedEvent):void
		{
			Automation.record(this, Command.create(ev.type, (object as TabNavigator).selectedIndex));
		}
		

		
		protected override function automationId():String
		{
			return "TabNavigator"+counter++;
		}
		/**
		 * Dispatch an event of type IndexChangedEvent.CHANGE on the connected ComboBox.
		 * @param index Index of the selected item 
		 */ 
		public function change(index:int):void
		{
			(object as TabNavigator).selectedIndex = index;
            // should not bubble (see AS doc of the event):
			// object.dispatchEvent(new IndexChangedEvent(IndexChangedEvent.CHANGE,false,false));
		}
	}
}