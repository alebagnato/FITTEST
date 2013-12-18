package eu.fittest.actionscript.automation.delegates.flex
{
	import flash.events.Event;
	import flash.events.MouseEvent;
	
	import mx.controls.sliderClasses.Slider;
	import mx.events.SliderEvent;
	import eu.fittest.actionscript.automation.Automation;
	import eu.fittest.actionscript.automation.Command;
	import eu.fittest.actionscript.automation.Delegate;
	
	/**
	 * Implementation of a Delegate for mx.controls.sliderClasses.Slider components
	 * It handles capture and replay of mx.events.SliderEvent events.
	 * 
	 * @see mx.controls.sliderClasses.Slider
	 * @see mx.events.SliderEvent
	 */ 
	public class SliderDelegate extends Delegate
	{
		private static var counter:int = 0;

		/**
		 * Create a SliderDelegate, which will listen for events of type SliderEvent.CHANGE on the
		 * given Slider.
		 * @param slider A Slider
		 */ 
		public function SliderDelegate(slider:Slider)
		{
			super(slider);
			slider.addEventListener(SliderEvent.CHANGE,changeHandler, false, -10);
		}	

		protected override function automationId():String
		{
			var slider:Slider = object as Slider;
            if (slider.automationName) return slider.automationName;
			if (slider.name) return slider.name;
			return "Slider"+counter++;
		}
		private function changeHandler(ev:SliderEvent):void
		{
			Automation.record(this, Command.create("change", ev.thumbIndex, ev.value));
		}
		
		/**
		 * Dispatch an event of type SliderEvent.CHANGE on the connected Slider.
		 //* @param index Index of the slider thumb to change
		 //* @param value New value for the slider 
		 * @param args: an array of <index,value> with the index of the slider thumb to change and the new value
		 */ 
		//public function change(index:int, value:int):void
		public function change(args: Array):void // by urueda
		{
			//(object as Slider).setThumbValueAt(index, value);
			(object as Slider).setThumbValueAt(args[0],args[1]); // by urueda
            // should not bubble (see AS doc of the event):
			//object.dispatchEvent(new SliderEvent(SliderEvent.CHANGE, false,false,index,value));
			object.dispatchEvent(new SliderEvent(SliderEvent.CHANGE, false,false,args[0],args[1])); // by urueda
		}
	}
}