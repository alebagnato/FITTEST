/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm.transformer;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import eu.fbk.se.fsm.cte.Class;
import eu.fbk.se.fsm.cte.Classification;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.CteObject;
import eu.fbk.se.fsm.cte.Marks;
import eu.fbk.se.fsm.cte.TestGroup;
import eu.fbk.se.fsm.cte.TestGroup.TestCase;
import eu.fbk.se.fsm.utils.CteUtils;
import eu.fbk.se.fsm.utils.FileUtil;
import eu.fbk.se.fsm.utils.JAXBUtil;

/**
 * Post process of the PPC for all tree to remove redundant coverage
 *  
 * @author cunduy
 *
 */
public class PostOptimizer {
	private static final int  M_SIZE = 50;
	
	Vector<DataClzIdentifier> globalClzIdentifiers = new Vector<DataClzIdentifier>();
	Cell[][] matrix = new Cell[M_SIZE][M_SIZE];
	
	Map<String, CteObject> map = new HashMap<String, CteObject>();
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String cteFolder = args[0];
		PostOptimizer optimizer = new PostOptimizer();
		optimizer.synthesizeInfo(cteFolder);
//		optimizer.printOutIdentifiers();
//		optimizer.printOut();
//		optimizer.printHTMLOut();
		
		System.out.println("Optimizing...");
		List<TestInfo> removedList = optimizer.postOptimizing();
		System.out.println("Number of removed test cases: " + removedList.size());
		for (TestInfo t : removedList){
			System.out.println("\t" + t.getPrettyId());
		}
		optimizer.removeTestCases(cteFolder, removedList);
	}
	
	/**
	 * Public method that caller can call to optimize trees 
	 * from the inputFolder, optimized trees are saved in
	 * the outputFolder
	 * 
	 * @param inputFolder
	 * @param outputFolder
	 */
	public boolean optimize(String inputFolder, String outputFolder){
		File input = new File(inputFolder);
		File output = new File(outputFolder);
		
		if (!input.exists() || !input.isDirectory()){
			return false;
		}
		
		if (!output.exists()){
			output.mkdirs();
		}
		
		if (!inputFolder.equals(outputFolder)){
			// copy all the trees to the output folder
			try {
				FileUtil.copyFolder(input, output);
			} catch (Exception e) {
				// TODO Auto-generated catch block
//				e.printStackTrace();
//				return false;
			}
		}
		
		String cteFolder = output.getAbsolutePath();
		synthesizeInfo(cteFolder);
		List<TestInfo> removedList = postOptimizing();
		removeTestCases(cteFolder, removedList);
	
		return true;
	}
	
	
	/**
	 * Synthesizing all information about the tree in the input folder
	 * 
	 * @param cteFolder
	 */
	public void synthesizeInfo(String cteFolder){
		// fill empty matrix
		for (int i = 0; i<M_SIZE; i++){
			for (int j = 0; j < M_SIZE; j++){
				matrix[i][j] = null;
			}
		}
		//clear data
		globalClzIdentifiers.clear();
		
		String[] listFile = FileUtil.listCTEfile(cteFolder);
		if (listFile != null){
			for (String s : listFile){
				String cteFile = cteFolder + File.separator + s;
				System.out.println("processing file " + cteFile);
				CteObject cteTree = JAXBUtil.loadCte(cteFile);
				if (cteTree!= null){
					map.put(s, cteTree);
					synthetizeTestInfo(cteTree, s);
				}
			}
		}
	}
	
	/**
	 * Once optimization is done, remove removable test cases
	 * 
	 * @param cteFolder
	 * @param removedList
	 */
	private void removeTestCases(String cteFolder, List<TestInfo> removedList) {
		for (TestInfo t : removedList){
			CteObject cteTree = map.get(t.treeId);
			if (cteTree != null){
				TestGroup testGroup = (TestGroup) cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().get(0);
				List<TestCase> toBeRemoved = new ArrayList<TestGroup.TestCase>();
				for (Object o : testGroup.getTestGroupOrTestCaseOrTestSequence()){
					if (o instanceof TestCase){
						TestCase tc = (TestCase)o;
						
						if (tc.getId().equals(t.testId)){
							toBeRemoved.add(tc);
						}
					}
				}
				testGroup.getTestGroupOrTestCaseOrTestSequence().removeAll(toBeRemoved);
				
				// save the tree back
				String cteFile = cteFolder + File.separator + t.treeId;
				if (testGroup.getTestGroupOrTestCaseOrTestSequence().size() > 1){
					JAXBUtil.saveCte(cteTree, cteFile);
				} else {
					// remove the file if no test case is there
					// FIXED Feb 2 20112, should still keep the file because 
					// it has the sequence
					// File f = new File(cteFile);
					// f.delete();
				}
			}
			
		}
	}

	/**
	 * Post optimizing the generated trees, taking into account
	 * the pair-wise coverage criteria to the global set of trees
	 * instead of local individual trees 
	 * 
	 * @return set of test cases that are removable
	 * 
	 */
	private List<TestInfo> postOptimizing() {
		List<TestInfo> retList = new ArrayList<TestInfo>();
		
		// flat the matrix to a list to make it easy to be sorted
		int size = globalClzIdentifiers.size();
		List<Cell> allCells = new ArrayList<Cell>();
		for (int i = 0; i < size; i++){
			for (int j = 0; j < size; j++){
				Cell c = matrix[i][j];
				if (c != null){
					allCells.add(c);
				}
			}
		}
		
		boolean done = false;
		while (!done){
			Collections.sort(allCells);
			done = true;
			for (Cell cell : allCells){
				Collections.sort(cell.tcList);
				TestInfo toBeRemoved = null;
				for (TestInfo test : cell.tcList){
					if (test.isRemovable()){
						done = false;
						retList.add(test);
						toBeRemoved = test;
						break;
					}
				}
				
				if (toBeRemoved != null){
					// remove the test case and restart the process
					remove(toBeRemoved);
					break;
				}
			}
		}
		return retList;
	}

	/**
	 * Remove a test case 
	 */
	private void remove(TestInfo toBeRemoved) {
		int size = globalClzIdentifiers.size();
		for (int i = 0; i < size; i++){
			for (int j = 0; j < size; j++){
				Cell c = matrix[i][j];
				if (c != null){
					if (c.tcList.contains(toBeRemoved)){
						c.tcList.remove(toBeRemoved);
					}
				}
			}
		}
	}

	/**
	 * Some print out methods used for debugging
	 */
	private void printOut() {
		int size = globalClzIdentifiers.size();
		System.out.println("Size = " + size);
		for (int i = 0; i < size; i++){
			for (int j = 0; j < size; j++){
				Cell c = matrix[i][j];
				if (c != null){
					System.out.println();
					System.out.println(globalClzIdentifiers.get(i).getPrettyId());
					System.out.println(globalClzIdentifiers.get(j).getPrettyId());
					for (TestInfo ti : c.tcList){
						System.out.println("\t\t" + ti.getPrettyId());
					}
				}
			}
		}
	}

	private void printOutIdentifiers() {
		int size = globalClzIdentifiers.size();
		System.out.println("Size = " + size);
		for (int i = 0; i < size; i++){
			System.out.println(globalClzIdentifiers.get(i).getPrettyId());
		}
	}

	private void printHTMLOut() {
		StringBuilder builder = new StringBuilder();
		builder.append("<table>\n");
		builder.append("<tr><td></td>");
		int size = globalClzIdentifiers.size();
		for (int i = 0; i < size; i++){
			builder.append("\t<td>");
			builder.append("\t" + globalClzIdentifiers.get(i).getPrettyId());
			builder.append("\t</td>\n");
		}
		builder.append("</tr>\n");
		
		for (int i = 0; i < size; i++){
			builder.append("<tr>\n");
			builder.append("\t<td>\n");
			builder.append("\t" + globalClzIdentifiers.get(i).getPrettyId());
			builder.append("\t</td>\n");
			for (int j = 0; j < size; j++){
				Cell c = matrix[i][j];
				builder.append("\t<td>");
				if (c != null){
					for (TestInfo ti : c.tcList){
						builder.append("\t\t" + ti.getPrettyId() + "<br>\n");
					}
				}
				builder.append("\t</td>\n");
			}
			builder.append("</tr>\n");
		}
		
		
		builder.append("</table>\n");
		
		System.out.println(builder.toString());
	}

	/**
	 * Synthesizing test case information from a tree 
	 * 
	 * @param cteTree
	 * @param treeId
	 */
	private void synthetizeTestInfo(CteObject cteTree, String treeId) {
		
		Composition testSequence = null;
		List<Object> oList = ((Composition) cteTree.getTree().getRoot())
				.getCompositionOrClassification();
		if (!oList.isEmpty()) {
			for (Object o : oList) {
				if (o instanceof Composition) {
					testSequence = (Composition) o;
					break; // first composition found is considered as test  sequence,
					// this list should have only one element
				}
			}
		}
		
		if (testSequence != null){
			List<Object> events = testSequence.getCompositionOrClassification();
		}
		
		// check if I should consider this tree
		int countParameterPair = 0;
		Vector<DataClzIdentifier> treeClzIdentifiers = new Vector<DataClzIdentifier>();
		List<Object> events = testSequence.getCompositionOrClassification();
		for (Object o : events){
			if (o instanceof Composition){
				if (!((Composition) o).getCompositionOrClassification().isEmpty()){
					Object firstClassification = ((Composition) o).getCompositionOrClassification().get(0);
					if (firstClassification instanceof Classification){
						Classification classification = (Classification) firstClassification;
						List<Class> listClz =  classification.getClazz();
						if (listClz.size() > 1){
							countParameterPair++;
							for (Class clz : listClz){
								DataClzIdentifier newIdentifier = new DataClzIdentifier();
								newIdentifier.sequenceId = ((Composition) o).getName();
								newIdentifier.paramId = classification.getName();
								newIdentifier.clzId = clz.getName();
								
								treeClzIdentifiers.add(newIdentifier);
							}
							
						}
					}
				}
			}
		}
		
		if (countParameterPair > 1){
			// update the data-class identifier list
			// at least a pair of parameters that has more than 2 data class each
			if (treeClzIdentifiers.size() > 0){
				for (DataClzIdentifier id : treeClzIdentifiers){
					if (!globalClzIdentifiers.contains(id)){
						globalClzIdentifiers.add(id);
					}
				}
			}
			
			// synthesize test case information
			TestGroup tg = (TestGroup) cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().get(0);
			// extract name - id map
			HashMap<String, String> clzIdName =  CteUtils.extractMapIdName(cteTree);
			for (Object obj : tg.getTestGroupOrTestCaseOrTestSequence()){
				if (obj instanceof TestCase){
					TestCase tc = (TestCase)obj;
//					if (tc.getValidity().equals(Validity.VALID)){
						StringBuilder testMarks = new StringBuilder();
						for (Marks marks : tc.getMarks()) {
							if (!marks.getTrue().isEmpty()) {
								testMarks.append(marks.getTrue());
							}
						}
						String[] sourceIDs = testMarks.toString().split(" ");
						
						for (String s1 : sourceIDs){
							for (String s2 : sourceIDs){
								if (!s1.equals(s2)){
									String dataClz1 = clzIdName.get(s1);
									String dataClz2 = clzIdName.get(s2);
									
									// check this pair
									DataClzIdentifier id1 = null; DataClzIdentifier id2 = null; 
									for (DataClzIdentifier id : treeClzIdentifiers){
										if (id.clzId.equals(dataClz1)){
											id1 = id;
										}
	
										if (id.clzId.equals(dataClz2)){
											id2 = id;
										}
									}
									
									if (id1 != null && id2 != null){
										// Good, we have a candidate
										// identify the corresponding cell
										int i = globalClzIdentifiers.indexOf(id1);
										int j = globalClzIdentifiers.indexOf(id2);
										
										if (matrix[i][j] == null){
											matrix[i][j] = new Cell();
										}
										
										TestInfo ti = new TestInfo();
										ti.treeId = treeId;
										ti.testId = tc.getId();
										
										matrix[i][j].tcList.add(ti);
										
									}
								}
							}
						}
//					}
				}
			}
		}
		
	}
	
	/**
	 * 
	 * @author cdnguyen
	 *
	 */
	private class DataClzIdentifier {
		public String sequenceId;
		public String paramId;
		public String clzId;
	
		@Override
		public boolean equals(Object obj) {
			if (obj instanceof DataClzIdentifier){
				if (sequenceId.equals(((DataClzIdentifier)obj).sequenceId)
						&& paramId.equals(((DataClzIdentifier)obj).paramId)
						&& clzId.equals(((DataClzIdentifier)obj).clzId)) {
					return true;
				} else {
					return false;
				}
			} else 
				return super.equals(obj);
		}

//		@Override
//		public int compareTo(DataClzIdentifier o) {
//			if (sequenceId.equals(o.sequenceId)
//					&& paramId.equals(o.paramId)
//					&& clzId.equals(o.clzId)) {
//				return 0;
//			} else {
//				return sequenceId.compareToIgnoreCase(o.sequenceId);
//			}
//		}
//		
		public String getPrettyId(){
			return sequenceId + "#" + paramId + "#" + clzId;
		}
	}
	
	/**
	 * Class that represent a test case
	 * 
	 * @author cdnguyen
	 *
	 */
	private class TestInfo implements Comparable<TestInfo>{
		public String treeId;
		public String testId;
		public String getPrettyId(){
			return treeId + "#" + testId;
		}
		
		@Override
		public boolean equals(Object obj) {
			if (obj instanceof TestInfo){
				if (((TestInfo) obj).treeId.equals(treeId)
						&& ((TestInfo) obj).testId.equals(testId))
					return true;
				else 
					return false;
				
			}
			return super.equals(obj);
		}

		@Override
		public int compareTo(TestInfo o) {
			if (o == null){
				return -1;
			} else {
				int myCov = this.getMyCoverage();
				int otherCov = o.getMyCoverage();
				if (myCov > otherCov){
					return 1;
				} else if (myCov < otherCov){
					return -1;
				} else {
					return 0;
				}
			}
		}
		
		public int getMyCoverage(){
			int counter = 0;
			int size = globalClzIdentifiers.size();
			for (int i = 0; i < size; i++){
				for (int j = 0; j < size; j++){
					Cell c = matrix[i][j];
					if (c != null){
						if (c.tcList.contains(this)){
							counter++;
						}
					}
				}
			}
			return counter;
		}
		
		public boolean isRemovable(){
			int size = globalClzIdentifiers.size();
			for (int i = 0; i < size; i++){
				for (int j = 0; j < size; j++){
					Cell c = matrix[i][j];
					if (c != null){
						if (c.tcList.contains(this)){
							if (c.tcList.size() == 1){
								return false;
							}
						}
					}
				}
			}
			
			return true;
		}
	}

	/**
	 * Class the represent a cell of the optimization matrix
	 * @author cdnguyen
	 *
	 */
	private class Cell implements Comparable<Cell>{
		
		public List<TestInfo> tcList = new ArrayList<TestInfo>();

		@Override
		public int compareTo(Cell o) {
			// descending
			if (o == null){
				return -1;
			} else {
				if (tcList.size() == o.tcList.size())
					return 0;
				else if (tcList.size() > o.tcList.size())
					return -1; 
				else 
					return 1;
				
			}
		}
	}
}
