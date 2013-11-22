package {
import flash.display.*;
import flash.events.*;
public class SimpleButtonDemo extends Sprite {
public function SimpleButtonDemo( ) {
// Create a simple button and configure its location
var button:SimpleButton = new SimpleButton( );
button.x = 20;
button.y = 20;
// Create the different states of the button, using the
// helper method to create different colors circles
button.upState = createCircle( 0x00FF00, 15 );
button.overState = createCircle( 0xFFFFFF, 16 );
button.downState = createCircle( 0xCCCCCC, 15 );
button.hitTestState = button.upState;
// Add an event listener for the click event to be notified
// when the user clicks the mouse on the button
button.addEventListener( MouseEvent.CLICK, handleClick );
// Finally, add the button to the display list
addChild( button );
}
// Helper function to create a circle shape with a given color
// and radius
private function createCircle( color:uint, radius:Number ):Shape {
var circle:Shape = new Shape( );
circle.graphics.lineStyle( 1, 0x000000 );
circle.graphics.beginFill( color );
circle.graphics.drawCircle( 0, 0, radius );
circle.graphics.endFill( );
return circle;
}
// Event handler invoked whenever the user presses the button
private function handleClick( event:MouseEvent ):void {

}
}
}
