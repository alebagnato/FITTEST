package eu.fbk.se.fsm.tcgenerator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import uk.co.demon.mcdowella.algorithms.AllPairs;

import eu.fbk.se.fsm.cte.Class;
import eu.fbk.se.fsm.cte.Classification;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.CteObject;
import eu.fbk.se.fsm.cte.Marks;
import eu.fbk.se.fsm.cte.TestGroup;
import eu.fbk.se.fsm.cte.TestGroup.TestCase;
import eu.fbk.se.fsm.utils.JAXBUtil;

public class PairWiseGenerator implements IAbstractTestCaseGenerator {
	// default parameter of AllPairs
	private static final boolean SHOULD_SHUFFLE = true;
	private static final int MAX_GOES = 100;
	private static final int MAX_TRY_FOR_IMPROVEMENT = 10;
	private static final long SEED = 42;

	private static final String GENERATOR_NAME = "PW-McDowell";

	/**
	 * Generate test cases for a tree using pairwise technique
	 * Generated test cases are stored in the tree 
	 * 
	 */
	@Override
	public boolean generateTestCases(CteObject cteTree) {
		if (cteTree == null)
			return false;

		List<ParamInfo> allParams = extractTreeInfo(cteTree);
		
		if (allParams.size() == 0)
			return false; // nothing to do
		
		int[][] result;
		if (allParams.size() > 1){

			int[] choices = new int[allParams.size()];
			for (int i = 0; i < choices.length; i++) {
				choices[i] = allParams.get(i).clzs.size();
			}
	
			AllPairs ap = new AllPairs(choices, SEED, SHOULD_SHUFFLE);
			int bestSofar = Integer.MAX_VALUE;
			int improvementCounter = MAX_TRY_FOR_IMPROVEMENT; // use to stop when no
																// improvement is
																// observed
			for (int go = 0;; go++) {
				switch (go % 2) {
				// Want to do the prime-based generation first as it may
				// be pretty if shuffling is turned off
				case 0:
					result = ap.generateViaPrime(false);
					break;
				case 1:
					result = ap.generateGreedy();
					break;
				default:
					throw new IllegalStateException("PairwiseGenerator: Bad case");
				}
	
				if (ap.minCount(result) < 1) {
					throw new IllegalStateException(
							"PairwiseGenerator: Generated bad result");
				}
				if (result.length < bestSofar) {
					bestSofar = result.length;
					improvementCounter = MAX_TRY_FOR_IMPROVEMENT; // reset
				} else if (result.length >= bestSofar) {
					improvementCounter--;
				}
				if (((MAX_GOES > 0) && (go >= MAX_GOES))
						|| (improvementCounter == 0)) {
					break;
				}
			}
		} else {
			// only one parameter
			result = new int[allParams.get(0).clzs.size()][1];
			for (int i = 0; i < allParams.get(0).clzs.size(); i++){
				result[i][0] = i;
			}
			
		}

		// process results, add test cases to cteTree
		TestGroup tg = new TestGroup();
		tg.setName(GENERATOR_NAME);
		int currentId = getMaxId(cteTree);
		tg.setId("c" + ++currentId);

		cteTree.getTestGroup().getTestGroupOrTestCaseOrTestSequence().add(tg); 
		
		for (int i = 0; i < result.length; i++) {
			int[] row = result[i];
			TestCase tc = new TestCase();
			tc.setId("c" + ++currentId);
			tc.setName("tc" + i);
			StringBuilder marks = new StringBuilder();
			for (int j = 0; j < row.length; j++){
				marks.append(allParams.get(j).clzs.get(row[j]));
				marks.append(" ");
			}
			Marks mrks = new Marks();
			mrks.setTrue(marks.toString());
			tc.getMarks().add(mrks);
			tg.getTestGroupOrTestCaseOrTestSequence().add(tc);
		}

		return false;
	}

	/**
	 * Get the max id to identify the next one, this method is really painful to run
	 * Another method could be traversing all the element, but checking 
	 * all node types is also tiring 
	 * 
	 * @param currentNode
	 * @param idPattern
	 * @return
	 */
	protected int getMaxId(CteObject tree) {

		JAXBContext ctx;
		try {
			ctx = JAXBContext.newInstance(CteObject.class);

			Marshaller marshaller = ctx.createMarshaller();

			DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder domBuilder = domFactory.newDocumentBuilder();
			Document doc = domBuilder.newDocument();
			marshaller.marshal(tree, doc);

			XPathFactory factory = XPathFactory.newInstance();
			XPath xpath = factory.newXPath();
			
			int max = -1;
			NodeList list = (NodeList) xpath.evaluate("//*[contains(@id, 'c')]", doc,
					XPathConstants.NODESET);
			for (int i = 0; i < list.getLength(); i++){
				Node n = list.item(i);
				int id = getNumericId("(c)([0-9]+)", n.getAttributes().getNamedItem("id").getNodeValue());
				if (id > max){
					max = id;
				}
			}
			return max;
		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XPathExpressionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			
		}
		return -1;
	}

	/**
	 * Get numeric id
	 * @param idPattern
	 * @param id
	 * @return
	 */
	protected int getNumericId(String idPattern, String id) {
		Pattern p = Pattern.compile(idPattern);
		Matcher m = p.matcher(id);
		if (m.find()) {
			if (m.groupCount() > 1) {
				try {
					Integer numericId = Integer.valueOf(m.group(2));
					return numericId.intValue();
				} catch (NumberFormatException e) {
					return -1;
				}
			}
		}
		return -1;
	}

	/**
	 * Extract information about the parameters and their classifications from a
	 * tree
	 * 
	 * @param cteTree
	 * @return
	 */
	protected List<ParamInfo> extractTreeInfo(CteObject cteTree) {
		Stack<Object> toVisit = new Stack<Object>();
		List<ParamInfo> retList = new ArrayList<PairWiseGenerator.ParamInfo>();

		toVisit.addAll(visit(cteTree.getTree().getRoot(), retList));
		while (!toVisit.isEmpty()) {
			Object o = toVisit.pop();
			toVisit.addAll(visit(o, retList));
		}

		return retList;
	}

	/**
	 * Visit a node in the tree, if the node is a composition the add its
	 * sub-nodes to the queue , otherwise, if the node is a classification then
	 * extract the corresponding parameter and its domain classifications
	 * 
	 * @param currentNode
	 * @param paramlist
	 * @return
	 */
	private Collection<? extends Object> visit(Object currentNode,
			List<ParamInfo> paramlist) {
		List<Object> objectToVisit = new ArrayList<Object>();
		if (currentNode instanceof Composition) {
			List<Object> oList = ((Composition) currentNode)
					.getCompositionOrClassification();
			if (!oList.isEmpty()) {
				for (Object o : oList) {
					if (o instanceof Composition) {
						objectToVisit.add(o);
					} else if (o instanceof Classification) {
						ParamInfo p = createParam((Classification) o);
						if (p != null) {
							paramlist.add(p);
						}

					}
				}

			}
		} else if (currentNode instanceof Classification) {
			ParamInfo p = createParam((Classification) currentNode);
			if (p != null) {
				paramlist.add(p);
			}
		}

		return objectToVisit;
	}

	/**
	 * Create a param from a classification
	 * 
	 * @param obj
	 * @return
	 */
	private ParamInfo createParam(Classification obj) {
		List<Class> clzList = obj.getClazz();
		if (!clzList.isEmpty()) {
			ParamInfo param = new ParamInfo();
			param.name = obj.getName();
			for (Class clz : clzList) {
				param.clzs.add(clz.getId());
			}
			return param;
		}
		return null;
	}

	/**
	 * Class to represent a parameter
	 * 
	 * @author cdnguyen
	 * 
	 */
	protected class ParamInfo {
		protected String name;
		protected List<String> clzs = new ArrayList<String>();
	}

	/**
	 * Main method for console use
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length == 2 ){
			
			String inputTree = args[0];
			String outputTree =  args[1];
			
			CteObject cteTree = JAXBUtil.loadCte(inputTree );
			PairWiseGenerator generator = new PairWiseGenerator();
			generator.generateTestCases(cteTree);
			
			JAXBUtil.saveCte(cteTree, outputTree);
		} else {
			System.err.println("Usage java eu.fbk.se.fsm.tcgenerator.PairWiseGenerator cte-input cte-output");
			System.exit(1);
		}
	}
	
}
