package eu.fittest.logging.trtransf.classAnalysis;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.text.Document;

public class PositionMapper {
	CompilationUnit taggedCu;
	CompilationUnit rewrittenCu;
	
	List<Integer> originalPosition  = new ArrayList<Integer>();
	List<Integer> rewrittenPosition = new ArrayList<Integer>();
		
	public PositionMapper(CompilationUnit taggedCu, Document rewrittenDoc) {
		
		this.taggedCu = taggedCu;
		// from rewritten document make CU;
		// this compilation unit will have the right line numbers in respect to its AST 
		ASTParser parser = ASTParser.newParser(AST.JLS3);
		parser.setSource(rewrittenDoc.get().toCharArray());
		this.rewrittenCu = (CompilationUnit)parser.createAST(null);
		
		this.compute();
	}	
	
	public void compute() {
		taggedCu.accept(new ASTVisitor() {
			public void preVisit(ASTNode n) {
				int l = taggedCu.getLineNumber(n.getStartPosition());
				originalPosition.add(l);
			}
		});
		
		rewrittenCu.accept(new ASTVisitor() {
			public void preVisit(ASTNode n) {
				int l = rewrittenCu.getLineNumber(n.getStartPosition());
				rewrittenPosition.add(l);
			}
		});
		
		//System.out.println(taggedCu);
		//System.out.println("----------------");
		//System.out.println(rewrittenCu);
		
		// we expect that the number of nodes in both compilation units is 
		// the same thus the number of line positions must be equal as well
		if (originalPosition.size() != rewrittenPosition.size())
			throw new Error("Count of computed line numbers is different: " + originalPosition.size() + " != " + rewrittenPosition.size());
		
		//for (int i = 0; i < originalPosition.size(); i++)
		//	System.out.println(originalPosition.get(i) + " > " + rewrittenPosition.get(i));
	}
	
	/**
	 * From original line number in application code returns line number
	 * corresponding to it in tagged application code
	 * @param lineNumber
	 * @return
	 */
	public int getRewrittenLineNumber(int lineNumber) {
		// we go through original positions to find the required line number
		for (int i = 0; i < originalPosition.size(); i++) {
			if (originalPosition.get(i) == lineNumber)
				return rewrittenPosition.get(i);
		}
		
		return -1;
	}
	/*
	public int getLineNumberOf(final ASTNode node) {
		
		// 1) find node in taggedCu and get its position (we get position before tagging)
		final List<Integer> result = new ArrayList<Integer>(); // here we store step value if found (we have to use List because Java is !@#$%) 
		
		taggedCu.accept(new ASTVisitor() {
			int steps = -1;
			boolean found = false;
			
			public void preVisit(ASTNode n) {
		
				//System.out.println(n.getClass());
				// if node already found => return
				if (found)
					return;
				
				// if we found node => stop searching and increasing 'steps'
				if (n == node) {
					found = true;
					result.add(steps);
					System.out.println("!!!!!!!!!!!FOUND");
				}
				System.out.println(node);
				// increase steps
				steps++;
			}
		});
		
		// node not found in original compilation unit
		if (result.size() == 0)
			throw new Error("Node position was not found in given compilation unit");
		
		// 2) now we know how many steps needs to be done in order to visit the 
		// required node; we go through the second compilation unit that has the same AST structure and moreover
		// it has the right line numbers in respect to the AST
		final List<ASTNode> foundNode = new ArrayList<ASTNode>(); // here we store found node
		
		rewrittenCu.accept(new ASTVisitor() {
			int steps = 0;
			boolean found = false;
			
			public void preVisit(ASTNode n) {
				// if node already found => return
				if (found)
					return;
				
				// if we are on the right position in the tree save the found node
				if (steps == result.get(0)) {
					found = true;
					foundNode.add(n);
				}
				
				// increase steps
				steps++;
			}
		});
		
		// node not found in rewritten compilation unit => steps is not correct
		if (foundNode.size() == 0)
			throw new Error("Step position in AST was not found");
		
		int lineNumber = rewrittenCu.getLineNumber(foundNode.get(0).getStartPosition());
		return lineNumber;
	}*/
}
