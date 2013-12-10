package gcd {

//import flash.display.Sprite;

public class Gcd {
  
  public function Gcd () {
    //recGcd(2,4);
    //whileGcd(6,8);
//     var x = 1;
//     LogUtils.logFCallEntry("test", x, "test");
//     LogUtils.logFCallExit("test", "void", x);
    //catchGcd(0,0);

    }

  
  public static function recGcd (x:int,y:int) {
    if (x == y) {
      LogUtils.logIf(true,"(x == y)");
      return x;
    }
    else if (x>y) {
      LogUtils.logIf(true,"(x > y)");
      return recGcd(x-y,y);
    }			
    else {
      LogUtils.logIf(false,"(x > y)");
      return recGcd(x,y-x);
    }
 }

  public static function whileGcd (x:int,y:int) {
    var iter_count:int = 0
    while (x != y) {
      iter_count++;
      if (x > y) {
	x = x - y;
      }
      else {
        y = y - x;
      }
    }
    LogUtils.logWhile("whileGcd", iter_count);
    trace ("whileGcd = " + x);
    return x;
  }

  public static function catchGcd (x:int,y:int) {
    try {  
      return Gcd.whileGcd(x,y);
    }
    catch (e:Error) {
      trace("catchGcd, handling exception: " + e.getStackTrace());
      return "check gcd args!"
    }
  }
}
} 
