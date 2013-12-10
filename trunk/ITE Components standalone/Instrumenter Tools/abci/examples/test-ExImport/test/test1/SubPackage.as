package test.test1 {
  public class SubPackage {
    private var var_sub_pack:String	;
    
    public var class_var:Function;
    
    private function printMessage():void {
      var_sub_pack = "nested package testing";
    }  
      
    public function notOverriddenFun():void {}
    
    protected function protectedMethod():void {
      //var_sub_pack = "protected method";
    }

    public function SubPackage():void {
      this.printMessage();
    }
  }
}