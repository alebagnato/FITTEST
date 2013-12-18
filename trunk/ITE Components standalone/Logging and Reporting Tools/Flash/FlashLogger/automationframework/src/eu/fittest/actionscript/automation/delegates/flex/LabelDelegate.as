package eu.fittest.actionscript.automation.delegates.flex
{
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mx.controls.Label;
	import eu.fittest.actionscript.automation.ClickableDelegate;
	
	/**
	 * Implementation of a Delegate for mx.controls.Label components
	 * It handles capture and replay of MouseEvent.CLICK events.
	 * 
	 * @see mx.controls.Label
	 * @see flash.events.MouseEvent
	 */ 

	public class LabelDelegate extends ClickableDelegate
	{
		private static var counter:int = 0;

		public function LabelDelegate(label:Label)
		{
			super(label);
		}	

		protected override function automationId():String
		{
			var label:Label = object as Label;
			if(label.name)
				return label.name;
			else if(label.text)
				return label.text;
			else if(label.automationName)
				return label.automationName;
			else return "Label"+counter++;
		}
	}
}