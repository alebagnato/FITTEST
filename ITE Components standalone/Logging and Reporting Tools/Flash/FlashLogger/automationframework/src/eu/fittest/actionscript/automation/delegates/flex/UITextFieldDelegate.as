package eu.fittest.actionscript.automation.delegates.flex
{
	import eu.fittest.actionscript.automation.Automation;
	import eu.fittest.actionscript.automation.ClickableDelegate;
	import eu.fittest.actionscript.automation.Command;
	
	import flash.events.Event;
	import flash.events.TimerEvent;
	import flash.utils.Timer;
	
	import mx.controls.TextInput;
		
	/**
	 * Implementation of a Delegate for mx.controls.TextInput components
	 * It handles capture and replay of flash.events.Event.CHANGE and
	 * MouseEvent.CLICK events.
	 * 
	 * @see mx.controls.TextInput
	 * @see flash.events.MouseEvent
	 * @see flash.events.Event
	 */ 
	public class UITextFieldDelegate extends ClickableDelegate
	{
		private static var counter:int = 0;
		private var typeTimer:Timer;
		
		/**
		 * Create a UITextFieldDelegate, which will listen for events of types Event.CHANGE and MouseEvent.CLICK on the
		 * given TextInput.
		 * Subsequent keystrokes (Event.CHANGE event) that are separated by time intervals of less than 1 second are accumulated into a single "type" Command, with
		 * the accumulated text of the TextInput as argument.
		 * 
		 * @param textinput A TextInput
		 * @see eu.fittest.actionscript.automation.Command 
		 */ 
		public function UITextFieldDelegate(textinput:TextInput)
		{
			super(textinput);
			typeTimer = new Timer(1000,1);
			typeTimer.addEventListener(TimerEvent.TIMER,textInputHandlerTimeOut, false, -10);
			textinput.addEventListener(Event.CHANGE,textInputHandler, false, -10);			
		}	
		private function textInputHandler(ev:Event):void
		{
				typeTimer.reset();
				typeTimer.start();
		}
		
		private function textInputHandlerTimeOut(ev:Event):void
		{
			Automation.record(this, Command.create("type", ( object as TextInput).text));
		}
		
				
		protected override function automationId():String
		{
			var field:TextInput = object as TextInput;
			if(field.id)
				return field.id + counter++;
			else 
				return "TextField"+counter++;
		}
		
		/**
		 * Set the text of the TextInput to a new string
		 * @param txt New text of the TextInput
		 */ 
		public function type(txt:String):void
		{
			(object as TextInput).text = txt;
            // should indeed bubble (see AS doc of the event):
			object.dispatchEvent(new Event(Event.CHANGE,true,false));
		}		
	}
}