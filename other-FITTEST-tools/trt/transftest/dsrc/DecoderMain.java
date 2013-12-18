import eu.fittest.tloglib.*;
import PkgA.* ;
import PkgA.PkgB.* ;

public class DecoderMain {

	public static void main(String[] args) throws Exception {
		String path = "../tlogGeneratedTagged" ;
		DLog.DEBUG = true ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Seq") ;
		DLog.copy = "" ;
		TestSeq.dec_main72() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)TestIf!") ;
		DLog.copy = "" ;
		TestIf.dec_main46() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)TestIfElse") ;
		DLog.copy = "" ;
		TestIfElse.dec_main50() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)TestAbstractMethod") ;
		DLog.copy = "" ;
		TestAbstractMethod.dec_main9() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Constructor1") ;
		DLog.copy = "" ;
		TestConstructor1.dec_main15() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Constructor2") ;
		DLog.copy = "" ;
		TestConstructor2.dec_main16() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Constructor3") ;
		DLog.copy = "" ;
		TestConstructor3.dec_main25() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)DoWhile") ;
		DLog.copy = "" ;
		TestDoWhile.dec_main27() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Exception1") ;
		DLog.copy = "" ;
		TestException1.dec_main29() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Expression") ;
		DLog.copy = "" ;
		TestExpression.dec_main35() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)ForLoop") ;
		DLog.copy = "" ;
		TestForLoop.dec_main44() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)LoopBreak") ;
		DLog.copy = "" ;
		TestLoopBreakContRet.dec_main55() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Overloading") ;
		DLog.copy = "" ;
		TestOverloading.dec_main62() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Polym") ;
		DLog.copy = "" ;
		TestPolymorh.dec_main66() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Switch") ;
		DLog.copy = "" ;
		TestSwitch.dec_main76() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)While") ;
		DLog.copy = "" ;
		TestWhile.dec_main86() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Try") ;
		DLog.copy = "" ;
		TestTry.dec_main82() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Package1") ;
		DLog.copy = "" ;
		TestPackage1.dec_main5() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)Package2") ;
		DLog.copy = "" ;
		TestPackage2.dec_main2() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
		
		DLog.loadOnPatternAndInitialize(path,"blog(.*)External") ;
		DLog.copy = "" ;
		TestExternalCall.dec_main39() ;
		DLog.closeDecoder() ;
		DLog.printDebug() ;
	}
	
}
