package {

public class Point {
   var x : int ;
   var y : int ;
   
   public function Point(x:int,y:int) {
      this.x = x ;
      this.y = y ;
   }
   
   public function isEqual(p : Point) :  Boolean {
      if (x != p.x) return false ;
      if (y != p.y) return false ;
      return true ;
   }

}
}