package code{

import flash.display.Sprite;
import flash.events.MouseEvent;


public class MyGame extends Sprite {
  private var selSquare : Square;

  public function MyGame() : void {
    addEventListener(MouseEvent.CLICK, clicked);
  }

  private function clicked(event:MouseEvent) : void {
    var x : int = event.localX;
    var y : int = event.localY;

    Log.clicked(x,y);
    var target  : Square  = getSquare(x,y);
    var taken   : Boolean = occupied(target);

    if (!this.selSquare && taken) {
      this.selSquare = target;
    } else if (this.selSquare && !taken) {
      Log.move(this.selSquare, target);
      move(this.selSquare, target);
      this.selSquare = null;
    }
  }

  private function move(from:Square,to:Square) : void {
  }

  private function getSquare(x:int,y:int) : Square {
	return null;
}

  private function occupied(sq:Square) : Boolean {
	return false;
}
}


}