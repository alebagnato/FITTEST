package eu.fittest.actionscript.automation.delegates.flex
{
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mx.controls.ComboBox;
	import mx.events.DropdownEvent;
	import mx.events.ListEvent;
	import eu.fittest.actionscript.automation.Automation;
	import eu.fittest.actionscript.automation.Command;
	import eu.fittest.actionscript.automation.Delegate;
	
	/**
	 * Implementation of a Delegate for mx.controls.ComboBox components
	 * It handles capture and replay of DropdownEvent.OPEN, DropdownEvent.CLOSE and ListEvent.CHANGE events.
	 * 
	 * @see mx.controls.ComboBox
	 * @see mx.events.DropdownEvent
	 * @see mx.events.ListEvent
	 */ 
	public class ComboBoxDelegate extends Delegate
	{
		private static var counter:int = 0;
		
		/**
		 * Create a ComboBoxDelegate, which will listen for events of types DropdownEvent.OPEN, 
		 * DropdownEvent.CLOSE and ListEvent.CHANGE on the
		 * given ComboBox.
		 * @param comboBox A ComboBox
		 */ 
		public function ComboBoxDelegate(comboBox:ComboBox)
		{
			super(comboBox);
			comboBox.addEventListener(DropdownEvent.OPEN,openCloseHandler, false, -10);
			comboBox.addEventListener(DropdownEvent.CLOSE,openCloseHandler, false, -10);
			comboBox.addEventListener(ListEvent.CHANGE,changeHandler, false, -10);
		}	

		protected override function automationId():String
		{
			var comboBox:ComboBox = object as ComboBox;
			if(comboBox.name)
				return comboBox.name;
			else if(comboBox.automationName)
				return comboBox.automationName;
			else return "ComboBox"+counter++;
		}
		private function changeHandler(ev:ListEvent):void
		{
			Automation.record(this, Command.create(ev.type, (object as ComboBox).selectedIndex));

		}
		private function openCloseHandler(ev:DropdownEvent):void
		{
			Automation.record(this, Command.create(ev.type));
		}
		/**
		 * Dispatch an event of type ListEvent.CHANGE on the connected ComboBox.
		 * @param index Index of the selected item 
		 */ 
		public function change(index:int):void
		{
			(object as ComboBox).selectedIndex = index;
            // should not bubble (see AS doc of the event):
			object.dispatchEvent(new ListEvent(ListEvent.CHANGE,false,false));
		}
		/**
		 * Dispatch an event of type DropdownEvent.OPEN on the connected ComboBox.
		 */ 
		public function open():void
		{
            // should not bubble (see AS doc of the event):
			object.dispatchEvent(new DropdownEvent(DropdownEvent.OPEN, false,false));
		}
		/**
		 * Dispatch an event of type DropdownEvent.CLOSE on the connected ComboBox.
		 */ 
		public function close():void
		{   // should not bubble (see AS doc of the event):
			object.dispatchEvent(new DropdownEvent(DropdownEvent.CLOSE, false,false));
		}
	}
}