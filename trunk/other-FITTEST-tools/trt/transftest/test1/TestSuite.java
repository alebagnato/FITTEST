
public class TestSuite {

	
	public static void main(String args[]) throws Exception {
		Long time0 = System.currentTimeMillis() ;
		TestAbstractMethod.main(null) ;
		TestConstructor1.main(null) ;
		TestConstructor2.main(null) ;
		TestConstructor3.main(null) ;
		TestDoWhile.main(null) ;
		TestException1.main(null) ;
		TestExpression.main(null) ;
		TestExternalCall.main(null) ;
		TestForLoop.main(null) ;
		TestIf.main(null) ;
		TestIfElse.main(null) ;
		TestLoopBreakContRet.main(null) ;
		TestOverloading.main(null) ;
		TestPolymorh.main(null) ;
		TestSeq.main(null) ;
		TestSwitch.main(null) ;
		TestTry.main(null) ;
		TestWhile.main(null) ;
		PkgA.TestPackage1.main(null) ;
		PkgA.PkgB.TestPackage2.main(null) ;
		Long time = System.currentTimeMillis() - time0 ;
		System.out.println("\n** TIME = " + time) ;
	}
}
