package coveragecollector;

import java.io.PrintStream;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
  * Universitat Politecnica de Valencia 2013
  * Camino de Vera, s/n
  * 46022 Valencia, Spain
  * www.upv.es    
  */

/**
  * author: Urko Rueda Molina
  * version 1.0
  * package coveragecollector
  */

public class CoverageTable {

	private boolean _equalDiffer; // true => equal; false => differ
	private Map<String,Map<Integer,CvgValue>> _cvgTable;
	private int _totalSize;
    private static DecimalFormat pf = new DecimalFormat("0.##%");

	public CoverageTable(boolean equalDiffer) {
		_cvgTable = new HashMap<String, Map<Integer,CvgValue>>();
		_equalDiffer = equalDiffer;
		_totalSize = 0;
	}

	public void initCoverageInfo(String file, Integer lineNo) {
		Map<Integer,CvgValue> lcTable = _cvgTable.get(file);
		if (lcTable == null) {
			lcTable = new HashMap<Integer,CvgValue>();
			_cvgTable.put(file,lcTable);
		}
		lcTable.put(lineNo, new CvgValue());
		_totalSize++;
	}

	/*public int getTotalSize() {
		int total = 0;
		for (Entry<String, Map<Integer,CvgValue>> fEntry : _cvgTable.entrySet())
		{
			total += fEntry.getValue().size();
		}
		return total;
	}*/
	
	public int getTotalSize() {
		return _totalSize;
	}
	
    public boolean coverageInfo(String info) {
    	String[] parts = info.split(":");
    	String file = parts[0];
        if(_cvgTable.containsKey(file)) {
        	Integer line = new Integer(parts[1]);
        	if (_cvgTable.get(file).containsKey(line) && !_cvgTable.get(file).get(line).isCvg) {
        		System.out.print("Covered: " + info);
        		CvgValue cvgV = _cvgTable.get(file).get(line);
        		cvgV.isCvg = true;
        		cvgV.cvgCount++;
        		return true;
        	}
        }
        return false;
    }	
    
    public synchronized void resetCoverageTable(boolean singleMultiple) {
    	CvgValue cvgValue;
        for (Entry<String, Map<Integer,CvgValue>> fEntry : _cvgTable.entrySet())
        {
        	for (Entry<Integer,CvgValue> lEntry : fEntry.getValue().entrySet()) {    	
    			cvgValue = lEntry.getValue();
    			if (singleMultiple) {
    				cvgValue.isCvg = Boolean.FALSE;
    				cvgValue.cvgSize++;
    			}
    			else {
    				cvgValue.reset();
    			}
        	}
        }
    }

	public synchronized void dumpRawTable(PrintStream out)
    {
        for (Entry<String, Map<Integer,CvgValue>> fEntry : _cvgTable.entrySet())
        {
        	for (Entry<Integer,CvgValue> lEntry : fEntry.getValue().entrySet()) {
        		out.println((_equalDiffer ? "_E_" : "_D_") + fEntry.getKey() + " @ " + lEntry.getKey() + " = " +
        	                (lEntry.getValue().isCvg ? "EXERCISED" : "NOT_EXERCISED"));
        	}
        }
    }

    public synchronized int dumpTable(PrintStream out, boolean acc)
    {
    	int count = 0;
    	if (_totalSize == 0) {
    		out.println("\nNo" + (_equalDiffer ? " equal " : " differing ") + "points to dump");
    	}
    	else {
	    	String leyend = (acc ? "([%] = accumulated coverage)" : "([#] = covered, [_] = not covered)");
			out.println("\nDump of" + (acc ? " accumulated" : "") + (_equalDiffer ? " equal " : " differing ") + "points coverage table " + leyend +
					    (_equalDiffer ? " (_E_ = equal point) ..." : " (_D_ = differing point"));
	        String mark = _equalDiffer ? "_E_" : "_D_";
	        int fileC, file = 0, tFiles = _cvgTable.size();
	        String lcString;
	        for (Entry<String, Map<Integer,CvgValue>> fEntry : _cvgTable.entrySet())
	        {
	        	fileC = 0;
	        	lcString = "";
	        	for (Entry<Integer,CvgValue> lEntry : fEntry.getValue().entrySet()) {
		            if (acc && lEntry.getValue().cvgCount > 0 || lEntry.getValue().isCvg) {
		                count++;
		                fileC++;			            	
		            }
		            lcString += lEntry.getKey() + "[" +
		            			(acc ? pf.format(((double)lEntry.getValue().cvgCount / lEntry.getValue().cvgSize))
		            				 : (lEntry.getValue().isCvg ? "#" : "_")) +
		            		    "] ";
	        	}
	        	file++;
	        	out.println("\n" + file + "/" + tFiles + ") " +
	        	            mark + fEntry.getKey() + " = " + fileC + " covered out of " + fEntry.getValue().size() +
	        			    " ( " + pf.format((double)fileC/fEntry.getValue().size()) + "):");
	        	out.println("\t" + lcString);
	        }
	        out.println("");
	        out.println("... dump of" + (_equalDiffer ? " equal " : " differing ") + "points coverage table done");
    	}
        return count;	        
    }    
    
    
    public static void printGlobalStats(int eS, int dS, PrintStream out) {
		int tS = eS + dS;
		out.println("-- Coverage points Number: " + tS + "\n" +
							"\tEqual points = " + eS + " out of " + tS + " (" + pf.format((double)eS/tS) + ")\n" +
							"\tDiffering points = " + dS + " out of " + tS + " (" + pf.format((double)dS/tS) + ")");
    	
    }
    
    public static void printTablesStats(int eCount, int dCount, int eTotal, int dTotal, PrintStream out) {
    	if (eCount == -1 || dCount == -1) {
    		System.out.println("Dump of tables report error - cannot ptint tables stats");
    		return;
    	}
    	int edCount = eCount + dCount;
    	int totalCount = eTotal + dTotal;
    	out.println("\n-- Total points coverage: " + edCount + " out of " +
        			totalCount + " (" + pf.format((double)edCount/totalCount) + ")");
    	if (eTotal > 0) {
    		out.println("\tEQUAL points coverage" + ": " + eCount + " out of " + eTotal +
    					" (" + pf.format((double)eCount/eTotal) + ")");
    	}
    	if (dTotal > 0) {
    		out.println("\tDIFFERING points coverage" + ": " + dCount + " out of " + dTotal +
    					" (" + pf.format((double)dCount/dTotal) + ")");
    	}
    	out.println("");
    	printGlobalStats(eTotal,dTotal,out);
    }
    
}
