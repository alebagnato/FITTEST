package eu.fittest.logging.trtransf.tagging;

import java.util.*;

import org.eclipse.jdt.core.dom.* ;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;

import eu.fittest.logging.trtransf.CodeTransformUtils;
import eu.fittest.logging.trtransf.classAnalysis.AnalysisUtils;
import eu.fittest.logging.trtransf.classAnalysis.BitGenerator;
import eu.fittest.logging.trtransf.classAnalysis.LogRelevance;
import eu.fittest.logging.trtransf.classAnalysis.MethodMapper;
import eu.fittest.logging.trtransf.classAnalysis.SignatureUtils;
import eu.fittest.tloglib.TLog;
 

/**
 * This is where logic of the tagging transformation is being defined.
 * It is defined in terms of a visitor class.
 *
 * @author Ales Sturala, Wishnu Prasetya
 *
 */
public class TaggingVisitor extends ASTVisitor {
	
	protected TaggingCodeGenerator context ;
	
	/**
	 * Class to tag.
	 */
	protected CompilationUnit cu ;
	
	// current class being processed; we'll need to keep this in a stack to
	// make it go-along with the tree-recursion
	private List<String> currentClass ;
	// current method decl being processed
	private String currentMethod ; 
	private int currentMethodID ;
		
	/**
	 * Constructor.
	 * 
	 * @param cu   The current target class to tag.
	 */
	public TaggingVisitor(CompilationUnit cu, TaggingCodeGenerator context) {
		this.cu = cu ;
		this.context = context ;
		currentClass = new LinkedList<String>() ;
	}
	
	public boolean visit(MethodInvocation node) { 
		return MethInvocationTagger.tagMethInv(context,node,
				currentClass.get(currentClass.size()-1),
				currentMethod,
				currentMethodID);
	}
		
	// super method invocation
	public boolean visit(SuperMethodInvocation node) { 
		return MethInvocationTagger.tagSuperMethInv(context,node);
	}
	
	public boolean visit(TypeDeclaration node) {
		currentClass.add(node.getName().getFullyQualifiedName()) ;
		return true ;
	}
	
	public void endVisit(TypeDeclaration node) {
		currentClass.remove(currentClass.size()-1) ;
	}
	
	public boolean visit(MethodDeclaration node) {						
		
		// Method declaration may have to be renamed!
		
		// check if method is log relevant
		String signature = SignatureUtils.getSignature(node.resolveBinding()) ;
		boolean isLogRelevant = context.getLRsignaturesInv().containsKey(signature) ;
				
		if (!isLogRelevant) return false;

		currentMethod = node.getName().getFullyQualifiedName() ;
		currentMethodID = context.getUniqueIDmap().get(node) ;
				
		// constructors are not renamed:
		if (node.isConstructor()) return true;
		// annotated top-logging methods are not renamed:
		if(hasTopLoggingMethodAnnotation(node)) {
			// we add a dummy exception handler at such a top level method:
			// this dummy handler is more complicated. E.g. the initLogger should
			// not not be put inside the dummy-try. So, for now I'm disabling it.
			//
			// addDummyExceptionHandler(node) ;
			return true ;
		}
		
		// "main" is not renamed:
		//System.out.println("##" + node + " is main = " + AnalysisUtils.isMainMethod(node)) ;
		if (AnalysisUtils.isMainMethod(node)) return true ;
		
		// in each method we implement a '_progress' indicator (except abstract methods)
		//if (node.getBody() != null)
			//InsertExecutionProgress(node);
		
		// in order to recognize log relevant !tracked! invocations (invocations we know about due to execution tracking)
		// from invocations non-tracked (e.g. implicit calls by Java, libraries, etc) we change name of the tracked invocations
		// to contain "_T" postfix;
		// first we generate method with the same original signature where in the body call
		// the renamed *_T method;
		AST ast = node.getAST();
		MethodDeclaration methodCopy = ast.newMethodDeclaration();
		
		// copy return type
		Type returnType = (Type)ASTNode.copySubtree(ast, node.getReturnType2());
		methodCopy.setReturnType2(returnType);
		
		// copy method name
		methodCopy.setName(ast.newSimpleName(node.getName().getFullyQualifiedName()));
		
		// copy modifiers
		for (Object o : node.modifiers()) {
			Modifier m = (Modifier) o;
			Modifier mCopy = (Modifier) ASTNode.copySubtree(ast, m);
			methodCopy.modifiers().add(mCopy);
		}				

		// copy exceptions
		for (Object e : node.thrownExceptions()) {
			Name exc1 = (Name) e ;
			Name exc2 = (Name) ASTNode.copySubtree(ast, exc1) ;
			methodCopy.thrownExceptions().add(exc2) ;
		}
		
		// copy method's parameters
		List<SingleVariableDeclaration> parameters = node.parameters(); 
		for (SingleVariableDeclaration p : parameters) {
			ASTNode newParameter = ASTNode.copySubtree(ast, p);
			methodCopy.parameters().add(newParameter);
		}
		
		// create method copy's body where the original (renamed) method is called
		// (unless the original method is abstract)
		Block methodCopyBody = node.getAST().newBlock();

		// method is not abstract
		if (node.getBody() != null) {
			MethodInvocation methodInvocation = ast.newMethodInvocation();
			methodInvocation.setName(ast.newSimpleName(node.getName().getFullyQualifiedName() + "_T"));

			// copy new method's arguments 
			for (SingleVariableDeclaration p : parameters)
				methodInvocation.arguments().add(ast.newSimpleName(p.getName().getFullyQualifiedName()));
			
			// generate TLog.untracedCall(..) call
			methodCopyBody.statements().add(getUntracedInvocationCall(node));
			
			// if method is void => do not use Return;
			if (node.getReturnType2().getClass().equals(PrimitiveType.class)
					&& ((PrimitiveType)node.getReturnType2()).getPrimitiveTypeCode().equals(PrimitiveType.VOID)) {
				ExpressionStatement es = ast.newExpressionStatement(methodInvocation);
				methodCopyBody.statements().add(es);
			}
			// use return
			else {
				ReturnStatement ret = ast.newReturnStatement();
				ret.setExpression(methodInvocation);
				methodCopyBody.statements().add(ret);
			}
			
			methodCopy.setBody(methodCopyBody);
		}
		// method is abstract
		else
			methodCopy.setBody(null);
		
		// insert new method into the class; 
		// we expect that the parent is TypeDeclaration
		TypeDeclaration parent = (TypeDeclaration)node.getParent();
		parent.bodyDeclarations().add(parent.bodyDeclarations().indexOf(node), methodCopy);
		
		// change name of the original method
		node.getName().setIdentifier(node.getName().getFullyQualifiedName() + "_T");

		return true;
	}
	
	private boolean hasTopLoggingMethodAnnotation(MethodDeclaration node) {
		IAnnotationBinding[] annotations = node.resolveBinding().getAnnotations() ;
		if (annotations==null || annotations.length==0) return false ;
		return annotations[0].getName().equals("TopLoggingMethodAnnotation") ;
	}
	
	private void addDummyExceptionHandler(MethodDeclaration node) {
		AST ast = node.getAST() ;
		Block body = node.getBody() ; 
		
		Block newBody = ast.newBlock() ;
		node.setBody(newBody) ;
		TryStatement tryStmt = ast.newTryStatement() ;
		newBody.statements().add(tryStmt) ;
		
		// constructing catch(Throwable e) { throw e ; }
		CatchClause catch0 = ast.newCatchClause() ;
		SingleVariableDeclaration exc0 = ast.newSingleVariableDeclaration() ;
		exc0.setName(ast.newSimpleName("dummyException")) ;
		exc0.setType(ast.newSimpleType(ast.newSimpleName("Error"))) ;
		catch0.setException(exc0) ;
		Block catchBody = ast.newBlock() ;
		ThrowStatement trw = ast.newThrowStatement() ;
		trw.setExpression(ast.newSimpleName("dummyException")) ;
		catchBody.statements().add(trw) ;
		catch0.setBody(catchBody) ;
		tryStmt.catchClauses().add(catch0) ;
		
		// put the old body as the body of the try:
		tryStmt.setBody(body) ;		
	}
	
	
	@SuppressWarnings("unchecked")
	public Statement getUntracedInvocationCall(MethodDeclaration method) {		
		AST ast = method.getAST();
		MethodInvocation inv = ast.newMethodInvocation();
		inv.setExpression(ast.newSimpleName("TLog"));
		inv.setName(ast.newSimpleName("untracedCall"));
		
		String packageName = method.resolveBinding().getDeclaringClass().getPackage().getName() ;
		String className   = currentClass.get(currentClass.size()-1) ;

		StringLiteral decoderName = ast.newStringLiteral() ;
		/*
		inv.arguments().add(ast.newNumberLiteral(Integer.toString(
				context.getMethodIDs().getMethodId(SignatureUtils.getSignature(method.resolveBinding())))));
		*/
		decoderName.setLiteralValue("" + packageName + "!" + className + "!dec_" + currentMethod + currentMethodID) ;
		inv.arguments().add(decoderName) ;
		ExpressionStatement es = ast.newExpressionStatement(inv);
		
		return es;
	}
	
	public boolean visit(InfixExpression node) {						// e1 && e2, e1 || e2
		// check if node is log relevant (that means none of its operators is log relevant)
		boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());
		if (!isLogRelevan) return false;
		
		AST ast = node.getAST() ;				
		// at least one of the two operators is log relevant
		
		// OPERATION e1 && e2
		Operator op = node.getOperator() ;
		if ((op == Operator.CONDITIONAL_AND) || (op == Operator.CONDITIONAL_OR)) {			
			// if e2 is log relevant
			if (LogRelevance.isLogRelevantNode(node.getRightOperand(),context.getLRsignatures_())) {			
				MethodInvocation push = mkPush(ast);
				Expression operand = node.getLeftOperand();
				node.setLeftOperand(push);
				push.arguments().add(operand);
			}
		}

		return true;
	}		

	public boolean visit(ConditionalExpression node) {					// (c ? e1 : e2)
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());
		if (!isLogRelevan) return false;

		// condition, e1 or e2 is log relevant;
		// check if e1 or e2 is log relevant
		if (LogRelevance.isLogRelevantNode(node.getThenExpression(),context.getLRsignatures_())
			|| LogRelevance.isLogRelevantNode(node.getElseExpression(),context.getLRsignatures_())) {
			
			MethodInvocation push = mkPush(node.getAST());
			Expression condition = node.getExpression();
			node.setExpression(push);
			push.arguments().add(condition);
		}
		
		return true;
	}
	
	public boolean visit(IfStatement node) {						
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());

		if (!isLogRelevan) return false;

		// condition, e1 or e2 is log relevant;
		// check if e1 or e2 is log relevant
		Statement S1 = node.getThenStatement() ;
		Statement S2 = node.getElseStatement() ;
		if (LogRelevance.isLogRelevantNode(S1,context.getLRsignatures_())
				|| 
				(S2 != null && LogRelevance.isLogRelevantNode(S2,context.getLRsignatures_()))) {
			
			MethodInvocation push = mkPush(node.getAST());
			Expression condition = node.getExpression();
			node.setExpression(push);
			push.arguments().add(condition);	
		}				

		return true;
	}

	public boolean visit(SwitchStatement node) {						// SWITCH
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());

		if (!isLogRelevan) return false;

		AST ast = node.getAST() ;
		
		// get switch's case blocks
		List<Statement[]> cases = CodeTransformUtils.getSwitchCases(node);
		
		int numberOfBitsRequired = BitGenerator.countBitsCombination(cases.size());
		BitGenerator bitsGenerator = new BitGenerator(numberOfBitsRequired) ;	 // generated unique combination of bits of specified length		

		// determines whether all cases in the switch are distinct
		Boolean isDistinctSwitch = CodeTransformUtils.isDistinctSwitch(node);

		// DISTINCT SWITCH
		if (isDistinctSwitch) {
			// System.out.println("** DISTINCT SWITCH") ;
			
			// tag each case block - add push() call at the beginning of each log relevant case block
			for (Statement[] c : cases) {	
				// find index of the 'case' statement in the switch and after
				// it put push() call
				int index = node.statements().indexOf(c[0]);
				// assuming the switch has been normalized:
				Block cb = (Block) node.statements().get(index+1) ;

				MethodInvocation push = mkPush(ast);
				StringLiteral argValue = node.getAST().newStringLiteral();
				String bits = bitsGenerator.next() ;
				argValue.setLiteralValue(bits);
				push.arguments().add(argValue);
				ExpressionStatement es = node.getAST().newExpressionStatement(push);
				//node.statements().add(index + 1, es);
				cb.statements().add(0,es) ;								
			}
		} 				
		// NOT-DISTINCT SWITCH
		else {
			// create new switch statement and for each 'case' block create corresponding
			// 'case' with only push() and break statements
			
			SwitchStatement newSwitch = ast.newSwitchStatement();
			ASTNode newSwitchExpr = ASTNode.copySubtree(newSwitch.getAST(), node.getExpression());
			newSwitch.setExpression((Expression)newSwitchExpr);
			
			// create cases
			List<Statement> newStatements = newSwitch.statements();

			for (Statement[] c : cases) {
				// case
				SwitchCase ss = ast.newSwitchCase();
				ASTNode caseExpr = ASTNode.copySubtree(ss.getAST(), ((SwitchCase)c[0]).getExpression());
				ss.setExpression((Expression)caseExpr);
				newStatements.add(ss);

				// push()
				MethodInvocation push = mkPush(ast);
				StringLiteral argValue = ast.newStringLiteral();

				// push unique bits combination
				String bits = bitsGenerator.next() ;
				argValue.setLiteralValue(bits);
				
				push.arguments().add(argValue);
				ExpressionStatement es = ast.newExpressionStatement(push);
				newStatements.add(es);
				// break
				BreakStatement brk = ast.newBreakStatement();
				newStatements.add(brk);
			}

			// add new switch in before the current switch statement;
			// we expect that switch can be only inside a 'block'
			Block block = (Block)node.getParent();
			
			// give me index of current switch in the block
			int index = block.statements().indexOf(node);

			// on the index insert the new switch, so the current 
			// switch moves to index + 1
			block.statements().add(index, newSwitch);
		}

		return true;
	}
	
	public boolean visit(WhileStatement node) {							// WHILE LOOP
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());

		if (!isLogRelevan) return false;

		final AST ast = node.getAST();
		int loopIdVariable = context.loopIdVariableGenerator++;
		
		// while statement is log relevant => change the guard to ipush(loopid,guard):
		MethodInvocation newguard = mkIpush(ast) ;
		Expression oldguard = node.getExpression() ;
		node.setExpression(newguard);
		// well... the parameters need to be set afterwards, due to reference
		// back to oldguard. Probably setExpression checks for circularity.
		newguard.arguments().add(ast.newSimpleName("loopId" + loopIdVariable));
		newguard.arguments().add(oldguard) ;
		
		// before current while statement put loop identificator:
		mkLoopIdAssignment(node,loopIdVariable) ;
		
		return true;
	}			
	
	
	private void mkLoopIdAssignment(ASTNode loop, int loopIdVariable) {
		AST astp =  loop.getParent().getAST() ;
		Block parent = (Block) loop.getParent();
		MethodInvocation getLoopId = CodeTransformUtils.mkMethodInvocation(
				astp,
				"getLoopId",
				astp.newSimpleName("TLog")) ;
		VariableDeclarationStatement vds = CodeTransformUtils.mkNewVar(
				astp,
				"loopId" + loopIdVariable, 
				astp.newPrimitiveType(PrimitiveType.LONG), 
				getLoopId ) ;	
		parent.statements().add(parent.statements().indexOf(loop), vds);
	}
	
	private void mkLoopBreakFlagAssignment(ASTNode loop, int loopIdVariable) {
		AST astp =  loop.getParent().getAST() ;
		Block parent = (Block) loop.getParent();
		VariableDeclarationStatement vds = CodeTransformUtils.mkNewVar(
				astp,
				"loopExitByBreak" + loopIdVariable, 
				astp.newPrimitiveType(PrimitiveType.BOOLEAN), 
				astp.newBooleanLiteral(false) ) ;	
		parent.statements().add(parent.statements().indexOf(loop), vds);
	}
		
	private void mkClosingPush(ASTNode loop, int loopIdVariable) {
		AST astp =  loop.getParent().getAST() ;
		Block parent = (Block) loop.getParent();
		
		MethodInvocation push = mkIpushWithBreakCheck(astp) ;
		push.arguments().add(astp.newSimpleName("loopExitByBreak" + loopIdVariable)) ;
		push.arguments().add(astp.newSimpleName("loopId" + loopIdVariable));
		
		int position = parent.statements().indexOf(loop) + 1 ;
		parent.statements().add(position,astp.newExpressionStatement(push)) ;				
	}

	// Make an ipush() call
	private MethodInvocation mkIpush(AST ast){
		return CodeTransformUtils.mkMethodInvocation(ast,"ipush",ast.newSimpleName("TLog")) ;
	}
	
	private MethodInvocation mkIpushWithBreakCheck(AST ast){
		return CodeTransformUtils.mkMethodInvocation(ast,"ipushWithBreakCheck",ast.newSimpleName("TLog")) ;
	}
	
	public boolean visit(DoStatement node) {							// DO-WHILE LOOP
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_()) ;
		if (!isLogRelevan) return false;
		
		AST ast  = node.getAST();
		int loopIdVariable = context.loopIdVariableGenerator++;

		// while statement is log relevant => add push() call to the condition
		MethodInvocation newguard = mkIpush(ast) ;
		Expression oldguard = node.getExpression() ;
		node.setExpression(newguard);
		newguard.arguments().add(ast.newSimpleName("loopId" + loopIdVariable));
		newguard.arguments().add(oldguard) ;
		
		// before current while statement put loop identificator:
		mkLoopIdAssignment(node,loopIdVariable) ;
		return true;
	}
	
	public boolean visit(ForStatement node) {							// FOR LOOP
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());

		if (!isLogRelevan) return false;
		
		final AST ast = node.getAST();
		int loopIdVariable = context.loopIdVariableGenerator++;
		
		// if neither condition, loop expression and body is log relevant (in other words,
		// only the initializer is log-relevant) => do not change the loop
		// System.out.println("** for statement: " + node) ;
		if ((node.getExpression() == null 
				||
				!LogRelevance.isLogRelevantNode(node.getExpression(),context.getLRsignatures_()))					// condition
			&& !LogRelevance.isLogRelevantNodeCollection(node.updaters(),context.getLRsignatures_())		// loop expression
			&& !LogRelevance.isLogRelevantNode(node.getBody(),context.getLRsignatures_())) {				// body
			return true;
		}

		// for statement has log relevant parts => add push() call to the condition
		// add push() call to the condition
		MethodInvocation newguard = mkIpush(ast) ;
		Expression oldguard = node.getExpression() ;
		if (oldguard==null) oldguard = ast.newBooleanLiteral(true) ;
		node.setExpression(newguard);
		newguard.arguments().add(ast.newSimpleName("loopId" + loopIdVariable));
		newguard.arguments().add(oldguard) ;
		
		// before current while statement put loop identificator:
		mkLoopIdAssignment(node,loopIdVariable) ;
		return true;
	}
	
	public boolean visit(EnhancedForStatement node) {
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());

		if (!isLogRelevan) return false;
		
		final AST ast = node.getAST();
		int loopIdVariable = context.loopIdVariableGenerator++;

		Block body = (Block) node.getBody() ;
		// add ipush(..,true) at the beginning of the body:
		MethodInvocation ipush = mkIpush(ast) ;
		body.statements().add(0,ast.newExpressionStatement(ipush)) ;
		ipush.arguments().add(ast.newSimpleName("loopId" + loopIdVariable));
		ipush.arguments().add(ast.newBooleanLiteral(true)) ;
		
		// before current while statement put loop identificator, and break-flagger:
		mkLoopIdAssignment(node,loopIdVariable) ;
		mkLoopBreakFlagAssignment(node,loopIdVariable) ;
		// insert break flagging:
		insertLoopBreakFlagger(body,"loopExitByBreak" + loopIdVariable) ;
		// add an extra guarded push(false) after the loop:
		mkClosingPush(node,loopIdVariable) ;
		return true;		
	}
	
	private void insertLoopBreakFlagger(Statement node, final String flagName) {
		        
		node.accept(new ASTVisitor(){ 
			public boolean visit(SwitchStatement node2) { return false ; }
			public boolean visit(WhileStatement node2){ return false ; }
			public boolean visit(ForStatement node2){ return false ; }
			public boolean visit(EnhancedForStatement node2){ return false ; }
			public boolean visit(DoStatement node2){ return false ; }
			
			public boolean visit(BreakStatement node2){
				Block parent = (Block) node2.getParent();
				AST astp = parent.getAST() ;
				Assignment asg = astp.newAssignment() ;
				asg.setLeftHandSide(astp.newSimpleName(flagName)) ;
				asg.setRightHandSide(astp.newBooleanLiteral(true)) ;
				ExpressionStatement asg_ = astp.newExpressionStatement(asg) ;
				parent.statements().add(parent.statements().indexOf(node2),asg_);
				return false ; 
				}
		}
		)
		;
	}

	public boolean visit(TryStatement node) {	
		// check if node is log relevant
		Boolean isLogRelevan = LogRelevance.isLogRelevantNode(node,context.getLRsignatures_());

		if (!isLogRelevan) return false;
		
		AST ast = node.getAST() ;
		
		// tick before entering the whole try-block:
		tickBeforeThisStatement(node) ;

		// tick at the end of the try-section:
		tickAtTheEndOfBlock(node.getBody()) ;
		
		for (Object catchClause_ : node.catchClauses()) {
			CatchClause c = (CatchClause) catchClause_ ;
			Block cbody = c.getBody() ;
			String exceptionVar = "" + c.getException().getName() ;
			// add logE(e) at the start of the catch-clause:
			MethodInvocation logE = mkLogE(ast) ;
			cbody.statements().add(0,ast.newExpressionStatement(logE)) ;
			logE.arguments().add(ast.newSimpleName(exceptionVar)) ;
			// tick at the end of each catch-clause:
			tickAtTheEndOfBlock(cbody) ;
		}
		
		Block finallyB = node.getFinally() ;
		if (finallyB != null) {
		   // add enterFinally at the beginning of the finally block:
		   MethodInvocation enterFinally = mkEnterFinally(ast) ;
		   finallyB.statements().add(0,ast.newExpressionStatement(enterFinally)) ;
		   // tick at the end of the finally-section:
		   tickAtTheEndOfBlock(finallyB) ;
		}
		
		// tag try block
		// ### FIXME exTagger.TagTryStatement(node);
		
		// increase progress when leaving try block
		//node.getBody().statements().add(getExecutionProgress(cu));
		return true;
	}
	
	private void tickAtTheEndOfBlock(Block b) {
		if (CodeTransformUtils.hasUnguardedReturnOrBreakOrThrow(b.statements())) return ;
		/*
		if (lastStatementIsBreakOrReturn(b)) return ;
		AST ast = b.getAST() ;
		if (lastStatementIsThrow(b)) {
			int N = b.statements().size() ;
			Statement last = (Statement) b.statements().remove(N-1) ;
			IfStatement fakeIf = ast.newIfStatement() ;
			fakeIf.setExpression(ast.newBooleanLiteral(true)) ;
			fakeIf.setThenStatement(last) ;
			b.statements().add(fakeIf) ;
		}
		*/
		AST ast = b.getAST() ;
		ExpressionStatement tick = ast.newExpressionStatement(mkTick(ast)) ;
		b.statements().add(tick) ;
	}
	
	private boolean lastStatementIsBreakOrReturn(Block b) {
		int N = b.statements().size() ;
		if (N==0) return false ;
		Statement last = (Statement) b.statements().get(N-1) ;
		return (last instanceof BreakStatement) || (last instanceof ReturnStatement) ;
	}
	
	private boolean lastStatementIsThrow(Block b) {
		int N = b.statements().size() ;
		if (N==0) return false ;
		Statement last = (Statement) b.statements().get(N-1) ;
		return (last instanceof ThrowStatement) ;
	}
			
	// Create a call TLog.push() 
	private MethodInvocation mkPush(AST ast) {
		return CodeTransformUtils.mkMethodInvocation(ast,"push",ast.newSimpleName("TLog")) ;
	}


	// Create a call TLog.tick()
	private  MethodInvocation mkTick(AST ast) {
		return CodeTransformUtils.mkMethodInvocation(ast,"tick",ast.newSimpleName("TLog")) ;
	}

	// Create a call TLog.logE()
	private  MethodInvocation mkLogE(AST ast) {
		return CodeTransformUtils.mkMethodInvocation(ast,"logE",ast.newSimpleName("TLog")) ;
	}
	
	// Create a call TLog.enterFinally()
	private  MethodInvocation mkEnterFinally(AST ast) {
		return CodeTransformUtils.mkMethodInvocation(ast,"enterFinally",ast.newSimpleName("TLog")) ;
	}
	
	public boolean visit (ReturnStatement node) {
		// this will only be visited if it is part of a 
		// log-relevant statement.
		// We tick before it:
		tickBeforeThisStatement(node) ;
		return true ;
	}
	
	public boolean visit (BreakStatement node) {
		// this will only be visited if it is part of a 
		// log-relevant statement.
		// We tick before it:
		tickBeforeThisStatement(node) ;
		return false ;
	}
	
	public boolean visit (ContinueStatement node) {
		// this will only be visited if it is part of a 
		// log-relevant statement.
		// We tick before it:
		tickBeforeThisStatement(node) ;
		return false ;
	}
	
	private void tickBeforeThisStatement(Statement stmt) {
		Block parent = (Block) stmt.getParent();
		AST astp = parent.getAST() ;
		ExpressionStatement tick = astp.newExpressionStatement(mkTick(astp)) ;
		parent.statements().add(parent.statements().indexOf(stmt),tick);
	}

}
