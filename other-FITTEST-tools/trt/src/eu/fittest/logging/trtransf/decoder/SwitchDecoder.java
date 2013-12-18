package eu.fittest.logging.trtransf.decoder;

import eu.fittest.logging.trtransf.*;
import eu.fittest.logging.trtransf.classAnalysis.*;
import eu.fittest.tloglib.DLog;

import java.util.*; 

import org.eclipse.jdt.core.dom.*;

public class SwitchDecoder {
	
	protected static List<Statement> generate(
			CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			SwitchStatement node) {
		
		List<Statement> ss = new ArrayList<Statement>();
		boolean isLogRelevant = LogRelevance.isLogRelevantNode(node, rootGenerator.getLRsignatures_());
		
		if (!isLogRelevant) return ss ;
		
		AST ast = node.getAST() ;
		
		// get switch's case blocks
		List<Statement[]> oldArms = CodeTransformUtils.getSwitchCases(node);
		int lengthOfBitStrEncoding = BitGenerator.countBitsCombination(oldArms.size());
		
		SwitchStatement newSwitch = ast.newSwitchStatement() ;
		newSwitch.setExpression(mkPOP(ast,className,lengthOfBitStrEncoding)) ;
		
		int switchNr = 0 ;
		for (Statement[] arm : oldArms) {
			
			// adding the switch-case:
			SwitchCase sw = ast.newSwitchCase() ;
			NumberLiteral k = ast.newNumberLiteral("" + switchNr) ;
			sw.setExpression(k) ;
			newSwitch.statements().add(sw) ;
			newSwitch.statements().addAll(decodeAcase(rootGenerator,orgCu,className,switchNr,arm[1])) ;
			switchNr++ ;
		}
		
		ss.add(newSwitch) ;
		return ss ;
	}
	
	
	// Constructing a pop(k) expression.
	protected static MethodInvocation mkPOP(AST ast, 
			String className,
			int bitStringLength)
	{
	
		MethodInvocation pop = ast.newMethodInvocation();
		pop.setExpression(ast.newSimpleName("DLog"));
		pop.setName(ast.newSimpleName("pop"));
		NumberLiteral k = ast.newNumberLiteral("" + bitStringLength) ;
		pop.arguments().add(k);
		return pop;
	}
	
	private static List<Statement> decodeAcase(CodeTransformer rootGenerator,
			CompilationUnit orgCu,
			String className,
			int armNr,
			Statement aCase) {
		
		List<Statement> ss = new ArrayList<Statement>();
		
		AST ast = aCase.getAST() ;
		
		// adding DLog.logLnNr(k)
		MethodInvocation logLnNr = ast.newMethodInvocation();
		logLnNr.setExpression(ast.newSimpleName("DLog"));
		logLnNr.setName(ast.newSimpleName("logLnNr"));
		
		StringLiteral lineNrInfo = ast.newStringLiteral() ;
		int caseStartLineNr = orgCu.getLineNumber(aCase.getStartPosition()) ;
		String className_  = (className == null || className =="") ? "?" : ("" + className) ;
		String caseLineNr_ = "" + className_ + ":" ;
		caseLineNr_ +=  (caseStartLineNr >= 0 ) ? caseStartLineNr : "?" ;
		
		lineNrInfo.setLiteralValue(caseLineNr_) ;
		logLnNr.arguments().add(lineNrInfo) ;
		
		ss.add(ast.newExpressionStatement(logLnNr)) ;
		
		// adding the arm's decoder:
		ss.addAll(StmtExprDecoder.generate(rootGenerator,orgCu,className,aCase)) ;

		return ss ;
	}

}
