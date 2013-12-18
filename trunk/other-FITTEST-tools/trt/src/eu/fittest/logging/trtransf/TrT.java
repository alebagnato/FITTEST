package eu.fittest.logging.trtransf;

import java.io.* ;
import java.util.*;

import org.eclipse.jdt.core.dom.* ;
import org.eclipse.jdt.core.* ;

import org.apache.commons.cli.* ;

import eu.fittest.logging.trtransf.classAnalysis.*;
import eu.fittest.logging.trtransf.tagging.* ;
import eu.fittest.logging.trtransf.normalization.* ;
import eu.fittest.logging.trtransf.decoder.* ;


/**
 * Providing the main executable implementaing traceability transformation.
 * @author Wishnu Prasetya
 *
 */
public class TrT {
	
	CommandLineParser parser ;
	Options options ;
	String  srcpath ;
	
	TrT() {
		parser = new GnuParser();
		options = new Options();
		
		Option srcO = new Option(null,"src",true,"The src dir.") ;
		srcO.setArgName("path") ;
		options.addOption(srcO) ;
		
		Option classO = new Option(null,"class",true,"One or more dirs or jars containing the classes.") ;
		classO.setArgName("path") ;
		options.addOption(classO) ;
		
		Option helpO = new Option(null,"help",false,"Print this message.") ;
		options.addOption(helpO) ;

		Option statO = new Option(null,"stat",false,"Print some basic statistics on the target codebase.") ;
		options.addOption(statO) ;
		
		Option tagO = new Option(null,"tag",false,"Produce tagged codebase.") ;
		tagO.setRequired(false) ;
		options.addOption(tagO) ;
		
		Option showO = new Option(null,"show",true,"Show the transformed versions of the files with the specified sufix.") ;
		tagO.setRequired(false) ;
		options.addOption(showO) ;
		
	}
	
	private String slashAtEnd(String s) {
		int n = s.length() - 1 ;
		if (s.charAt(n) != '/') return s + "/" ;
		return s ;
	}
	
	private String normalizeSlash(String s) {
		return s.replace('\\', '/') ; 
	}
	
	/**
	 * This is where the command line arguments are interpreted.
	 */
	void interpretArgs(String[] args) throws Exception {
		
		CommandLine cmd = parser.parse(options, args) ;
		
		if (cmd.hasOption("help")) {
			HelpFormatter h = new HelpFormatter() ;
			h.printHelp(70, "TrT --root <path> (other-option)*", "", options, "") ;
			return ;
		}
		
		if (!cmd.hasOption("src")) throw new ParseException("src path is missing.") ;
		String srcpath = cmd.getOptionValue("src") ;
		srcpath = slashAtEnd(normalizeSlash(srcpath)) ;
		
		String[] binpaths = { srcpath }  ;
		if (cmd.hasOption("class")) {
			binpaths = cmd.getOptionValue("class").split(";") ; 
		}
		//for (int i=0; i<binpaths.length;i++)
		//	System.out.println("##" + binpaths[i]) ;
		
		// load up the files:
		String targets[] = IOutils.getJavaSourceNames(srcpath) ; 
		List<CompilationUnit> cus = IOutils.getCompilationUnits(srcpath, binpaths,targets) ;
		
		if (cmd.hasOption("stat")) { // print statistics
			AnalysisUtils.analyzeClass(cus) ;
			List<MethodDeclaration> z = LogRelevance.getLogRelevantMethods(cus) ;
		    System.out.println("\n** Log relevant methods: ") ;
		    for (MethodDeclaration m : z) {
		    	System.out.println("  " + SignatureUtils.getFullMethodName(m.resolveBinding())) ;
		    }
		}
		
		if (cmd.hasOption("tag")) { // do tagging
			System.out.println("\n** Normalizing...") ;
			Normalizer normalizer = new Normalizer(cus) ;
			normalizer.transform() ;
			TaggingCodeGenerator tagger = new TaggingCodeGenerator(cus) ;		
			System.out.println("\n** Tagging...") ;
		    tagger.transform() ;
		    if (cmd.hasOption("show")) {
		    	String suffix = cmd.getOptionValue("show") ;
		    	if (suffix.equals("*")) suffix = "" ;
		    	for (int i=0; i<targets.length; i++) {
		    		if ((targets[i]).endsWith(suffix))
		    			System.out.println("\n** TAGGED version:\n\n" + cus.get(i)) ;
		    	}
		    }
		    System.out.println("\n** Saving tagged files...") ;
		    String projectdir =  slashAtEnd(normalizeSlash(IOutils.getParentPathName(srcpath))) ;
		    String taggedSrcPath = projectdir + "tsrc" ;
		    saveCompilationUnits(taggedSrcPath,cus) ;
		    System.out.println("** tagging DONE.") ;
		    
		    // reload the compilation units:
		    cus = IOutils.getCompilationUnits(srcpath, binpaths,targets) ;
		    System.out.println("\n** Normalizing...") ;
			normalizer = new Normalizer(cus) ;
			normalizer.transform() ;
			DecoderGenerator dgen = new DecoderGenerator(cus) ;
			System.out.println("\n** Generating decoders...") ;
			List<CompilationUnit> decoders = dgen.transform() ;
			String decoderSrcPaths = projectdir + "dsrc" ;
			//for (CompilationUnit d : decoders) {
			//	System.out.println("## decoder: " + d) ;
			//}
			saveCompilationUnits(decoderSrcPaths,decoders) ;
			System.out.println("** generating decoders DONE.") ;
		}
				
	}
	
	private void saveCompilationUnits(String srcpath, List<CompilationUnit> cus) 
		throws Exception
	{
		for (CompilationUnit c : cus) {
			if (c.types().isEmpty()) continue ;
	    	String pckName = IOutils.getPckgName(c) ;
	    	String className = IOutils.getClassName(c) ;
	    	System.out.println("   Saving " + pckName + "." + className + " ...") ;
	    	IOutils.saveJavaClass(srcpath, c) ;
	    }
	}
	
	
	public static void main(String[] args) {
	   try {
	   (new TrT()).interpretArgs(args) ;
	   }
	   catch (Exception e) {
		   throw new Error(e) ;
	   }
	}
	
	/*
	public static void main__(String[] args) throws Exception {
		
		// Environment set up:
		String srcpath = "D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/transftest/test1" ;
		String[] binpaths = { "D:/workshop/eclipseWorkspace/tlogTransTest_v1/bin" ,
			                  "D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/build/loglib.jar"	              
				            } ;
		String targets[] = IOutils.getJavaSourceNames(srcpath) ; // {"Hello.java", "A/A.java", "A/A2.java" } ;	
		List<CompilationUnit> cus = IOutils.getCompilationUnits(srcpath, binpaths, targets) ;

		AnalysisUtils.analyzeClass(cus) ;
	    
	    List<MethodDeclaration> z = LogRelevance.getLogRelevantMethods(cus) ;
	    System.out.print("* Log relevant methods: ") ;
	    for (MethodDeclaration m : z) {
	    	System.out.print("  " + SignatureUtils.getFullMethodName(m.resolveBinding())) ;
	    }
	    
	    TaggingCodeGenerator tagger = new TaggingCodeGenerator(cus) ;
	    tagger.transform() ;
	    
	    System.out.println("\n TAGGED version:\n" + cus.get(0)) ;
	    //System.out.println("\n TAGGED version:\n" + cus.get(1)) ;
	    //System.out.println("\n TAGGED version:\n" + cus.get(2)) ;

	}
	*/

}
