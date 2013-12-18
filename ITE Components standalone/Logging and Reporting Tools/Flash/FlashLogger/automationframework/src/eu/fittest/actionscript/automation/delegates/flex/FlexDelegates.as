package eu.fittest.actionscript.automation.delegates.flex
{
	
	import flash.display.Sprite;
	
	import mx.controls.Button;
	import mx.controls.ButtonBar;
	import mx.containers.TabNavigator;
	import mx.controls.ComboBox;
	import mx.controls.Label;
	import mx.containers.Canvas;
	import mx.controls.Image;
	import mx.controls.TextInput;
	import mx.controls.sliderClasses.Slider;
	import eu.fittest.actionscript.automation.Automation;
	import flash.utils.*;

	/**
	 * Entry point of the FlexDelegates library
	 */ 
	public class FlexDelegates extends Sprite
	{
		/**
		 * Entry point of the FlexDelegates library. Registers all delegates implemented by the library.
		 * 
		 * @see eu.fittest.actionscript.automation.Automation#registerDelegateClass
		 */ 
		public function FlexDelegates()
		{
			Automation.registerDelegateClass(ButtonBar, ButtonBarDelegate);
			Automation.registerDelegateClass(Button, ButtonDelegate);
			Automation.registerDelegateClass(TabNavigator, TabNavigatorDelegate);
			Automation.registerDelegateClass(ComboBox, ComboBoxDelegate);
			Automation.registerDelegateClass(Label, LabelDelegate);
			//Automation.registerDelegateClass(Canvas, CanvasDelegate);
			Automation.registerDelegateClass(Image, ImageDelegate);  
			Automation.registerDelegateClass(Slider, SliderDelegate);
			Automation.registerDelegateClass(TextInput, UITextFieldDelegate);
		}
	}
}