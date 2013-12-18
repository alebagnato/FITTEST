package eu.fittest.actionscript.automation.delegates.flex
{
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.utils.*;
	
	import mx.controls.Button;
	import eu.fittest.actionscript.automation.ClickableDelegate;

	/**
	 * Implementation of a Delegate for mx.controls.Button components
	 * It handles capture and replay of MouseEvent.CLICK events.
	 * 
 	 * @see mx.controls.Button
	 * @see flash.events.MouseEvent
	 */ 

	public class ButtonDelegate extends ClickableDelegate
	{
		private static var counter:int = 0;

		public function ButtonDelegate(button:Button)
		{
			super(button);
		}	

		protected override function automationId():String
		{
			var button:Button = object as Button;
			// trace("** id: " + button.automationName);  
			if (button.id) 
			{
			    return button.id + counter++;
			}
			else
			{
			  if (button.label)
			  {
			     return button.label;
			  }
			  else
			  {
			    return "Button"+counter++; 
			  }
			}
		}
	}
}