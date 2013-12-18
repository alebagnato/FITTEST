package eu.fittest.actionscript.automation.delegates.flex
{
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mx.containers.Canvas;
	import eu.fittest.actionscript.automation.ClickableDelegate;
	
	/**
	 * Implementation of a Delegate for mx.containers.Canvas components
	 * It handles capture and replay of MouseEvent.CLICK events.
	 * 
	 * @see mx.containers.Label
	 * @see flash.events.MouseEvent
	 */ 

	public class CanvasDelegate extends ClickableDelegate
	{
		private static var counter:int = 0;

		public function CanvasDelegate(canvas:Canvas)
		{
			super(canvas);
		}	

		protected override function automationId():String
		{
			var canvas:Canvas = object as Canvas;
			if(canvas.name)
				return canvas.name;
			else if(canvas.automationName)
				return canvas.automationName;
			else return "Canvas"+counter++;
		}
	}
}