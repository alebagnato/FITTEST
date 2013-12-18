package eu.fittest.actionscript.automation.delegates.flex
{
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mx.controls.Image;
	import eu.fittest.actionscript.automation.ClickableDelegate;
	
	/**
	 * Implementation of a Delegate for mx.controls.Image components
	 * It handles capture and replay of MouseEvent.CLICK events.
	 * 
	 * @see mx.controls.Image
	 * @see flash.events.MouseEvent
	 */ 

	public class ImageDelegate extends ClickableDelegate
	{
		private static var counter:int = 0;

		public function ImageDelegate(image:Image)
		{
			super(image);
		}	

		protected override function automationId():String
		{
			var image:Image = object as Image;
			  if(image.id)
			      return image.id + counter++;
			  else 	
			      return "Image"+counter++;
		}
	}
}