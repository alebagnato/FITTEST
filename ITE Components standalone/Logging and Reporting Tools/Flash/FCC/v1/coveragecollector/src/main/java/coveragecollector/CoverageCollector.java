package coveragecollector;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.net.ServerSocket;

/**
  * Universitat Politecnica de Valencia 2013
  * Camino de Vera, s/n
  * 46022 Valencia, Spain
  * www.upv.es    
  */

/**
  * authors: Arthur Baars and Urko Rueda Molina
  * version 1.0
  * package coveragecollector
  */

public class CoverageCollector
{
	
	public static final String USER_COVERAGE_TYPE = "user";
	public static final String TESTCASE_COVERAGE_TYPE = "testcase";
	
	private static String _cvgType;
	private static String _cvgFile;
	private static String _rptFile; // report dump file
	private static String _rpwFile; // RAW report dump file
	private static String _cvgParent;
	
	//public static final String COVERAGE_FILE = "reports/coverageDump.txt";
	private static int _nDumps = 1;

	private static CoverageTable _equalCvgTable, _differCvgTable; // for equal and differing points between source versions

	public CoverageCollector() {
        _equalCvgTable = new CoverageTable(true);
        _differCvgTable = new CoverageTable(false);
    }
    
    public static void main(String[] args) throws IOException
    {
    	if (checkCallErrors(args)) {
    		callError();
    		return;
    	}

    	_cvgType = args[0];
    	int port = new Integer(args[1]).intValue();
    	_cvgFile = args[2];

    	File cvgF = new File(_cvgFile);
    	String parent = cvgF.getParent() + "/reports/" + System.currentTimeMillis() + "/";
		new File(parent).mkdirs(); // make reports directories
    	
    	String file = cvgF.getName();
    	_rptFile = parent + file.replace(".txt", "_" + _cvgType + ".rpt");
    	_rpwFile = parent + file.replace(".txt", "_" + _cvgType + ".rpw");
    	
    	// check cvg parent and set
		_cvgParent = "/";
    	String cvgParent = new File(_cvgFile).getParentFile().getName();
    	if (cvgParent != null && !cvgParent.isEmpty()) {
    		File cvgParentF = new File("reports/" + cvgParent); 
    		if (cvgParentF.exists()) {
				_cvgParent = "/" + cvgParent + "/";
    		}
    		else {
    			if (cvgParentF.mkdir()) {
    				_cvgParent = "/" + cvgParent + "/";
    			}
    			else {
    				System.out.println("Directory - " + cvgParentF.getAbsolutePath() + "- could not be created!");
    			}
    		}
    	}
	
		CoverageCollector collector = new CoverageCollector();
		collector.readCoveragePointsFile(_cvgFile);
	
		ServerSocket serversocket = new ServerSocket(port);
		new Thread(new ServerThread(collector, serversocket)).start();
	
		CoverageTable.printGlobalStats(_equalCvgTable.getTotalSize(),
									   _differCvgTable.getTotalSize(),
									   System.out);
		printInfo() ;

		int cx = System.in.read() ;
		while(cx != 'x') {
			if      (cx == 'f')  printAll(); 
			else if (cx == 'a')  printTables(System.out,true);
			else if (cx == 'c')  printTables(System.out,false);
			else if (cx == 'r') {_equalCvgTable.resetCoverageTable(false) ;
								 _differCvgTable.resetCoverageTable(false) ;}
			else if (cx == '\n') printInfo() ;               
			cx = System.in.read() ;
		 }
		
		serversocket.close();
				
    }

    
    static void printInfo() {
        System.out.println("\n-- Type < x , ENTER > to eXit the collector") ;
        System.out.println("--      Close the SUT to dump the coverage data to files, or:") ;
        System.out.println("--      < f , ENTER > to FORCE the dump of the coverage data to files") ;    
        System.out.println("--      < a , ENTER > to dump the ACCUMULATED coverage data to Screen") ;    
        System.out.println("--      < c , ENTER > to dump the CURRENT     coverage data to Screen") ;    
        System.out.println("--      < r , ENTER > to reset the CURRENT and ACCUMULATED coverage data (0% coverage)") ;    
     }
    
    /**
   	 * Checks errors in the call (parameters).
     * @param args Call parameters.
     * @return 'true' if any errors in the parameters.
     */
    private static boolean checkCallErrors(String[] args) {
    	System.out.println("");
    	// check parameters number
        if(args.length < 3) { // coverage_type, port, converage_points_file
        	System.out.println("Not enough parameters");
            return true;
        }
        // check coverage_type
        String cType = args[0];
        if (!cType.equals(USER_COVERAGE_TYPE) && !cType.equals(TESTCASE_COVERAGE_TYPE)) {
        	System.out.println("Invalid coverage type");
        	return true;
        }
        // check port
        try {
            int port = new Integer(args[1]).intValue();
            if (port < 1) {
            	System.out.println("Port number less than 1");
            	return true;
            }
        } catch (java.lang.Exception e) {
        	System.out.println("Port not a number");
        	return true;
        }
        // check coverage_points_file
        File cvgfile = new File(args[2]);
        if (!cvgfile.exists()) { // we assume the file is correct (but should check it!)
        	System.out.println("Coverage points file does not exist");
        	return true;
        }
                
    	return false; // all parameters ok
    }
    
    private static void callError() {
    	System.err.println("--");
        System.err.println("Usage: CoverageCollector <coverage-type> <port-number> <coverage points file>");
        System.err.println("Example: \tCoverageCollector user 12345 ../flexstore_coverage/flexstore1/coveragePointsClean.txt");
        System.err.println("Example: \tCoverageCollector testcase 12345 ../flexstore_coverage/flexstore1/coveragePointsExtended.txt");    	
    	System.err.println("--\n");
    }

    /* this is the old version:
    private void readCoveragePointsFile(String fn) throws IOException {
        File coveragePoints = new File(fn);
        CSVReader reader = new CSVReader(new FileReader(coveragePoints), '\t');
        String[] line ;
        while((line = reader.readNext()) != null){
          String file = line[0];
          String lineNo = line[1];

          String name = file + ":" + lineNo;
          //System.out.println("** " + name) ;
          name = name.intern();
          table.put(name, Boolean.FALSE);                
        }
    }
    */
    
    private void readCoveragePointsFile(String fn) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(fn));
        String line = null, file;
        Integer lineNo;
        boolean differ;
        String[] parts;
        while ((line = reader.readLine()) != null) {
		  parts = line.split("\\s");
		  file = parts[0] ;
		  lineNo = new Integer(parts[1]);
		  differ = false;
		  if (parts.length > 2 && parts[2].contains("_DIFFER_")) {
			  differ = true;
		  }
		  //String name = file + ":" + lineNo;
		  //name = name.intern();
		  if (differ) {
			  _differCvgTable.initCoverageInfo(file,lineNo);
		  }
		  else { 
			  _equalCvgTable.initCoverageInfo(file,lineNo);
		  }
        }
        reader.close();
    }
    
    private static void printAll() {
    	printSingle(); printAccumulated();
    }
    
    private static void printSingle() {
    	print(_rptFile.replace(".rpt",  "_" + _nDumps + ".rpt"),
   	          _rpwFile.replace(".rpw", "_" + _nDumps + ".rpw"),
   	          false);    	
    }
    
    private static void printAccumulated() {
    	print(_rptFile,_rpwFile,true);    	
    }
    
    private static void print(String rpt, String rpw, boolean acc) {
    	String cvgRange = acc ? "Accumulated" : "Single";
    	try {
	    	PrintStream rptOut = new PrintStream(rpt);
    		printTables(rptOut,acc);
            if (rpt != null)
            	System.out.println("** " + cvgRange + " coverage data has been saved to " + rpt) ;
    	} catch (IOException e) {
    		System.out.println("Error during dump to: " + rpt == null ? "Screen" : rpt);
    		e.printStackTrace();    		
    	}
    	try {
    		PrintStream rpwOut = new PrintStream(rpw);
            _equalCvgTable.dumpRawTable(rpwOut) ;
            _differCvgTable.dumpRawTable(rpwOut) ;    
        	if (rpw != null)
        		System.out.println("** " + cvgRange + " coverage RAW data has been saved to " + rpw) ;            
    	} catch (IOException e) {
    		System.out.println("Error during RAW dump to: " + rpw == null ? "Screen" : rpw);
    		e.printStackTrace();
    	}
    }
    
    private static void printTables(PrintStream out, boolean acc) {
	    CoverageTable.printTablesStats(_equalCvgTable.dumpTable(out,acc),
									   _differCvgTable.dumpTable(out,acc),
									   _equalCvgTable.getTotalSize(),
									   _differCvgTable.getTotalSize(),
									   out);
    }    
    
    public synchronized void coverageInfo(String info) {
    	if (_equalCvgTable.coverageInfo(info))
    		System.out.println("_EQUAL-point_");
    	else if (_differCvgTable.coverageInfo(info))
    		System.out.println("_DIFFERING-point_");
    	else
            System.err.println("Warning: unknown coverage point '" +info+"'");
    }
    
    public synchronized void coverageThreadClosed() {
    	System.out.println("Coverage THREAD CLOSED");
    	printAll();
    	_equalCvgTable.resetCoverageTable(true);
    	_differCvgTable.resetCoverageTable(true);
    	_nDumps++;
    	System.out.println("Single coverage has been reset");
    	// trace dump
    	//System.out.println("#" + new File(reportF).getAbsolutePath() + "#");
    }
    
}