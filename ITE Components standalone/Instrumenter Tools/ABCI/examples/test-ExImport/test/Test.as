package test {

  import test.test1.*;

  public class Test extends SubPackage implements ITest  {
    public var pub_test_var:String;
    private var priv_test_var:String;
    
    public function packFun():void {
    }

    override protected function protectedMethod():void {
    } 
    
    public function intFun1():void {}

    function Test(){
      trace("call to constructor");
    }
  }
}