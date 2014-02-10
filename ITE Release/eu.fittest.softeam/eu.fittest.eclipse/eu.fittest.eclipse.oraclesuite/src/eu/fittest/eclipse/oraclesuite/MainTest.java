package eu.fittest.eclipse.oraclesuite;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.fittest.eclipse.oraclesuite.actions.PerlScripts;

public class MainTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String logFolderPath = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\samples\\flexstoreCaseStudy\\iteration2\\flexstore3\\sampledeeplogs";
		
		File logFolder = new File(logFolderPath);
		
		String _GHRCTopts = "";
		 String _oracleFile = "oracle.inv";
		 String _reportFile = "report.txt";
		 String _violationFile = "violations.txt";
		 List<String> _functionsToInclude = new ArrayList<String>();
		 _functionsToInclude.add("");
		 
		 String _otherLLOoptions = "";
		 
		 String eventsToInclude_regexp = "";
		 List<String> fieldsToInclude = new ArrayList<String>();
		 fieldsToInclude.add("numOfSelectedItems");
		 fieldsToInclude.add("numInShopCart");
		 fieldsToInclude.add("cartTotal");
		 fieldsToInclude.add("selectedProduct");
		 fieldsToInclude.add("catalogContents");
		 fieldsToInclude.add("shoppingCartContents");
		 fieldsToInclude.add("compareCartContents");
		 
		 String oldLogFolderPath = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\samples\\flexstoreCaseStudy\\iteration2\\flexstore2\\samplelogs";
		 File oldLogFolder = new File(oldLogFolderPath);
		 
		 String newLogFolderPath = "D:\\Projects\\FITTEST\\fittest-svn\\Software\\samples\\flexstoreCaseStudy\\iteration2\\flexstore3\\samplelogs";
		 File newLogFolder = new File(newLogFolderPath);

		
		PerlScripts.inferllo(oldLogFolder,  _GHRCTopts, _oracleFile, _reportFile, _functionsToInclude, _otherLLOoptions);
//		PerlScripts.inferllo(newLogFolder,  _GHRCTopts, _oracleFile, _reportFile, _functionsToInclude, _otherLLOoptions);
//		PerlScripts.checkllo(oldLogFolderPath,  newLogFolderPath, _oracleFile, _violationFile, _reportFile);
//		
//		PerlScripts.inferDaikon(oldLogFolderPath,  _GHRCTopts, _oracleFile, _reportFile, eventsToInclude_regexp, fieldsToInclude);
//		PerlScripts.inferDaikon(newLogFolderPath,  _GHRCTopts, _oracleFile, _reportFile, eventsToInclude_regexp, fieldsToInclude);
//		PerlScripts.checkDaikon(oldLogFolderPath,  newLogFolderPath, _oracleFile, _violationFile, _reportFile);
//		
		System.err.println("Finished");
		
	}

}
