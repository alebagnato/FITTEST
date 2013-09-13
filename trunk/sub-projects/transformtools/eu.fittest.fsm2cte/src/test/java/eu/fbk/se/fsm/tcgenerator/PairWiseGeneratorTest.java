package eu.fbk.se.fsm.tcgenerator;

import static org.junit.Assert.*;

import java.io.File;
import java.util.List;

import org.junit.Test;

import eu.fbk.se.fsm.cte.CteObject;
import eu.fbk.se.fsm.tcgenerator.PairWiseGenerator.ParamInfo;
import eu.fbk.se.fsm.utils.JAXBUtil;

public class PairWiseGeneratorTest {

	@Test
	public void extractTreeTest() {
		String inputTree = System.getProperty("user.dir") 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "data"
				+ File.separator + "p0.cte";

		CteObject cteTree = JAXBUtil.loadCte(inputTree);
		
		PairWiseGenerator generator = new PairWiseGenerator();
		List<ParamInfo> l =  generator.extractTreeInfo(cteTree);
		assertNotNull(l);
		assertTrue(l.size() == 8);
		assertTrue(l.get(0).clzs.size() == 6);
		
		for (ParamInfo p : l){
			System.out.println(p.name + ", size = " + p.clzs.size());
			for (String s : p.clzs){
				System.out.println("\t" + s);
			}
		}
	}
	
	@Test
	public void generateTestCasesWithBigTree(){
		String inputTree = System.getProperty("user.dir") 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "data"
				+ File.separator + "bigtree.cte";
		
		CteObject cteTree = JAXBUtil.loadCte(inputTree);
		
		PairWiseGenerator generator = new PairWiseGenerator();
		
		int oldSize = cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().size();
		generator.generateTestCases(cteTree);
		
		int newSize = cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().size();
		
		assertTrue(newSize == oldSize + 1);
		
		String outputTree = System.getProperty("user.dir") 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "data"
				+ File.separator + "bigtree-with-tcs.cte";
		JAXBUtil.saveCte(cteTree, outputTree);
	}
	
	@Test
	public void generateTestCasesTest() {
		String inputTree = System.getProperty("user.dir") 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "data"
				+ File.separator + "p0.cte";
		
		CteObject cteTree = JAXBUtil.loadCte(inputTree);
		
		PairWiseGenerator generator = new PairWiseGenerator();
		
		int oldSize = cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().size();
		generator.generateTestCases(cteTree);
		
		int newSize = cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().size();
		
		assertTrue(newSize == oldSize + 1);
		
		String outputTree = System.getProperty("user.dir") 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "data"
				+ File.separator + "p0-with-tcs.cte";
		JAXBUtil.saveCte(cteTree, outputTree);
		
	}

	@Test
	public void extractTreeMaxIDTest() {
		String inputTree = System.getProperty("user.dir") 
				+ File.separator + "src"
				+ File.separator + "test"
				+ File.separator + "data"
				+ File.separator + "p0.cte";
		
		CteObject cteTree = JAXBUtil.loadCte(inputTree);
		PairWiseGenerator generator = new PairWiseGenerator();
		int maxId = generator.getMaxId(cteTree);
		System.out.println(maxId);
		
	}

}
