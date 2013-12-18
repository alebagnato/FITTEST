package eu.fittest.logging.trtransf;

import java.io.*;
import java.nio.file.* ;
import java.nio.file.Path;
import java.util.*;


import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.dom.* ;
import org.eclipse.core.runtime.* ;

public class IOutils {

	/**
	 * Get the string content of a text file.
	 */
	static public String getFileContent(String fileName) throws Exception {
	    File f = new File(fileName);
	    byte[] bytes = new byte[(int) f.length()];
	    InputStream in = new FileInputStream(f);
	    int m = 0, n = 0;
	    while (m < bytes.length) {
		   n = in.read(bytes, m, bytes.length - m);
		   m += n;
	    }
	    in.close();
	    return new String(bytes); 
	}
	
	/**
	 * Get the relative paths of all Java sources in the given target path.
	 */
	static public String[] getJavaSourceNames(String path) throws Exception {
		Path path_ = FileSystems.getDefault().getPath(path) ;
		List<String> r = getJavaSourceNames(path_,path_) ;
		String[] files = new String[r.size()] ;
		int i = 0 ;
		for (String f : r) { files[i] = f ; i++ ; } 
		return  files ;
	}
	
	static private List<String> getJavaSourceNames(Path root, Path path) throws Exception {
		
		List<String> contents = new LinkedList<String>() ;
		
		DirectoryStream<Path> stream = Files.newDirectoryStream(path) ;
		
	    for (Path entry: stream) {
	        if (entry.toFile().isDirectory()) {
	        	contents.addAll(getJavaSourceNames(root,entry)) ;
	        	continue ;
	        }
	        if (entry.toString().endsWith(".java")) {
	        	String f = root.relativize(entry).toString() ;
	        	contents.add(f) ;
	        }
	    }
	    
	    stream.close() ;
		
		return contents ;
	}
	
	static public String getParentPathName(String path) {
		File f = new File(path) ;
		return f.getParent() ;
	}
	
	static public String getClassName(CompilationUnit cu) {
		assert ! cu.types().isEmpty() ;
		TypeDeclaration cu_ = (TypeDeclaration) cu.types().get(0) ;
		return cu_.getName().toString() ;
	}
	
	static public String getPckgName(CompilationUnit cu) {
		PackageDeclaration pck = cu.getPackage() ;
		if (pck == null) return "" ;
		return pck.getName().toString() ;
	}
	
	static public void saveJavaClass(String srcpath, CompilationUnit cu) throws Exception {
		String content = "" + cu ;
		String pckPath = getPckgName(cu).replace('.', '/') ;
		String className = getClassName(cu) ;
		String pckFullPath = slashAtEnd(slashAtEnd(srcpath) + pckPath) ;
		// (new File(pckFullPath)).mkdir() ;
		createPckgDirs(srcpath,getPckgName(cu)) ;
		String fpath = pckFullPath + className + ".java" ;
		File cf = new File(fpath) ;
		//System.out.println("#### " + fpath) ;	
		if (!cf.exists()) cf.createNewFile() ;	
		FileWriter fw = new FileWriter(cf);
		fw.write(content) ;
		fw.close() ;
	}
	
	static private void createPckgDirs(String srcpath, String pckPath) {
		String path = "" + srcpath + "/" ;
		String s = "" + pckPath ;
		//System.out.println("#### package path name: " + s) ;	
		while (!s.isEmpty()) {
			int k = s.indexOf('.') ;
			if (k<0) {
				//System.out.println("#### creating dir: " + path + s) ;	
				(new File(path + s)).mkdir() ;
				break ;
			}
			else {
				path += s.substring(0,k) ;
				//System.out.println("#### creating dir: " + path) ;	
				(new File(path)).mkdir() ;
				path += "/" ;
				s = s.substring(k+1) ;
			}
		}
	}
	
	
	static private String slashAtEnd(String s) {
		int n = s.length() - 1 ;
		if (s.charAt(n) != '/') return s + "/" ;
		return s ;
	}
	
	/**
	 * Get the AST of a given java source file. This also requires the full
	 * path to the source-root where the file is part of to be given, and
	 * also the path to the bin-root of the corresponding class files.
	 * 
	 * @param srcpath  The path to the source-root where the target resides.
	 * @param binpath  The path to the corresponding bin-root. 
	 * @param target   The relative path, wrt source-root, to the target java, e.g. A/B/hello.java.
	 * @return         The AST of the target.
	 * @throws Exception 
	 */
	static public CompilationUnit getCompilationUnit(String srcpath, String[] binpath, String target) 
		   throws Exception 
    {
		String fulltargetpath = srcpath + "/" + target ;	
		String input = getFileContent(fulltargetpath) ;
		
		// creating a parser object:
		ASTParser parser = ASTParser.newParser(AST.JLS3) ; // // handles JDK 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6
		parser.setSource(input.toCharArray()) ;
		
		// Setting up a project-like environment for the parser, which it needs to
		// do binding resolution. The latter is expensive.. but we need it.
		//String mytlog = "D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/transftest/libs" ;
		String[] srcpath_ = { srcpath } ;
		String[] encoding  = null ; // documentation not clear what this is..., or how to set it
		parser.setEnvironment(binpath,srcpath_,encoding,false) ;
		parser.setUnitName(target) ;
		
		setParserOptions(parser) ;

		// Now parse:
		CompilationUnit cu = (CompilationUnit) parser.createAST(null);	
		// this allow modifications to be recorded, and later on applied to
		// get a new AST:
		cu.recordModifications(); 
		
		return cu ;
	}
	
	static public List<CompilationUnit> getCompilationUnits(
			   String srcpath, 
			   String[] binpath, 
			   String[] targets) 
			   throws Exception 
	    {
		   List<CompilationUnit> results = new ArrayList<CompilationUnit>() ;
		   for (int i=0; i<targets.length; i++)
			   results.add(getCompilationUnit(srcpath,binpath,targets[i])) ;
		   return results ;	
	    }
	
	static private void setParserOptions(ASTParser parser) {
		// setting up compilation options:
		parser.setKind(ASTParser.K_COMPILATION_UNIT);
		// In order to parse 1.5 code, some compiler options need to be set to 1.5
		Map options = JavaCore.getOptions();
		JavaCore.setComplianceOptions(JavaCore.VERSION_1_5, options);
		parser.setCompilerOptions(options);
		
		// configuring, to turn on binding resolution:
		parser.setResolveBindings(true);
		//parser.setBindingsRecovery(true) ;
	}
	
	
	/*
	 * 
	 * SUSPENDED: does not work...
	static public List<CompilationUnit> xgetCompilationUnit(String srcpath, String binpath, String[] targets) {

		final List<CompilationUnit> cus = new ArrayList<CompilationUnit>() ;
		FileASTRequestor requestor = new FileASTRequestor() {
			public void acceptAST(String sourceFilePath, CompilationUnit ast) {
				System.out.println("** constructing AST of " + sourceFilePath) ;
				cus.add(ast) ;
			}		    
		} ;
		
		ASTParser parser = ASTParser.newParser(AST.JLS3) ; 
		
		String[] srcpath_ = { srcpath } ;
		String[] binpath_ = { binpath } ;
		String[] encodings = null ;
		String[] bindingKeys = {} ;
		parser.setEnvironment(binpath_,srcpath_,encodings,false) ;
		setParserOptions(parser) ;
		parser.createASTs(targets, encodings, bindingKeys, requestor, new NullProgressMonitor()) ;
		return new ArrayList<CompilationUnit>(cus) ;
	}
	*/
	
	public static void main(String[] args) throws Exception {
		String[] files = getJavaSourceNames("D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/test/") ;
		for (int i=0; i<files.length; i++)
			System.out.println(files[i]) ;
	}

}
