package fbk.se.services.mutation;

import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;

import fbk.se.services.mutation.Mutator;
import fbk.se.services.mutation.utils.FileUtils;

public class MutatorTest {
	Mutator mutator;
	String projectPath = System.getProperty("user.dir");
	String constraintFile = projectPath + "/xsd/EbayFindingAPIConstraints.xml";

	@Before
	public void init() {
		mutator = new Mutator(constraintFile);
	}

	/*
	 * @Test public void mutateTest(){ String xmlFile = projectPath +
	 * "/../ebay-finder/etc/sniffer/fs-dom/1287413112993/RES.xml"; Document doc
	 * = FileUtils.loadXMLfileToDOMDoc(xmlFile); Assert.assertNotNull(doc);
	 * List<Document> mutants = mutator.mutate(doc);
	 * Assert.assertNotNull(mutants);
	 * 
	 * // save to file for (int i = 0; i < mutants.size(); i++){ Document mutant
	 * = mutants.get(i); String outFileName = projectPath +
	 * "/../ebay-finder/etc/sniffer/fs-dom/1287413112993/" + "RES-MU" +
	 * String.valueOf(i) + ".xml"; FileUtils.saveDOMDocToFile(mutant,
	 * outFileName); } }
	 */

	@Test
	public void testMutate() {
		String xmlFile = projectPath + "/test-data/RES.xml";
		Document doc = FileUtils.loadXMLfileToDOMDoc(xmlFile);
		Assert.assertNotNull(doc);
		List<Document> mutants = mutator.mutate(doc);
		Assert.assertNotNull(mutants);

		// save to file
		for (int i = 0; i < mutants.size(); i++) {
			Document mutant = mutants.get(i);
			String outFileName = projectPath + "/test-data/" + "RES-MU"
					+ String.valueOf(i) + ".xml";
			FileUtils.saveDOMDocToFile(mutant, outFileName);
		}

		// XXX Assertion: inspect the output file manually
	}
}
