package fbk.se.mutation;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ContainerNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import fbk.se.mutation.constraints.*;
import fbk.se.mutation.json.operators.IMutationOperator;
import fbk.se.mutation.json.operators.MutationFactory;
import fbk.se.mutation.utils.FileUtils;
import fbk.se.mutation.utils.JsonUtils;
import fbk.se.mutation.utils.PathException;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

public class JsonMutator {
	private static final String JAXB_CONSTRAINT_PACKAGE = "fbk.se.mutation.constraints";
	private String constraintsSpecFile;
	private Constraints constraints = null;

	private Map<String, List<OnElementType>> mutationDB;

	public JsonMutator(String constraintFile) {
		this.constraintsSpecFile = constraintFile;
		init();
	}

	/**
	 * Initiate constraints
	 */
	private void init() {
		try {
			JAXBContext jct = JAXBContext.newInstance(JAXB_CONSTRAINT_PACKAGE);
			Unmarshaller um = jct.createUnmarshaller();
			constraints = (Constraints) um.unmarshal(new File(
					constraintsSpecFile));
			mutationDB = new HashMap<String, List<OnElementType>>();

			for (String mutationType : IMutationOperator.XSD_MUTATION_TYPES) {
				List<OnElementType> list = new ArrayList<OnElementType>();
				mutationDB.put(mutationType, list);
			}

			for (OnElementType element : constraints.getOnElement()) {
				if (element.getRestriction() != null) {
					ComplexRestrictionType restriction = element
							.getRestriction();

					// constraint facets
					List<Object> facets = restriction.getFacets();
					for (Object o : facets) {
						List<OnElementType> list = null;
						if (o instanceof JAXBElement) {
							String constraintType = ((JAXBElement) o).getName()
									.getLocalPart();
							list = mutationDB.get(constraintType);
							if (list != null)
								list.add(element);
							if (constraintType
									.equals(IMutationOperator.XSD_MUTATION_ENUMERATION)) {
								// there are usually more than one enumeration,
								// so exit this for to
								// avoid duplications
								break;
							}
						} else if (o instanceof Pattern) {
							list = mutationDB
									.get(IMutationOperator.XSD_MUTATION_PATTERN);
							if (list != null)
								list.add(element);
						} else if (o instanceof WhiteSpace) {
							list = mutationDB
									.get(IMutationOperator.XSD_MUTATION_WHITESPACE);
							if (list != null)
								list.add(element);
						} else if (o instanceof TotalDigits) {
							list = mutationDB
									.get(IMutationOperator.XSD_MUTATION_TOTALDIGITS);
							if (list != null)
								list.add(element);
						}
					}

				}
			}

		} catch (JAXBException e) {
			e.printStackTrace();
			constraints = null;
		}
	}

	/**
	 * Mutate a Json document, return a new one with mutated data
	 * 
	 * If mutation failed (due to constraint problem or whatever) return null
	 * 
	 * @param input
	 * @return
	 */
	public List<JsonNode> mutate(JsonNode input) {
		// no constraint ready, return null
		if (constraints == null)
			return null;

		List<JsonNode> retList = new ArrayList<JsonNode>();

		for (String mutationType : IMutationOperator.XSD_MUTATION_TYPES) {
			List<JsonNode> tmp = mutate(input, mutationType);
			if (tmp != null && tmp.size() > 0) {
				retList.addAll(tmp);
			}
		}

		return retList;
	}

	/**
	 * Mutate a document based on a specific mutation type
	 * 
	 * @param inputDoc
	 * @param mutationType
	 * @return
	 */
	public List<JsonNode> mutate(JsonNode inputDoc, String mutationType) {
		// no constraint ready, return null
		if (constraints == null)
			return null;

		List<OnElementType> constraintSpecs = mutationDB.get(mutationType);
		if (constraintSpecs == null || constraintSpecs.size() == 0) {
			return null; // no specs
		}

		IMutationOperator operator = MutationFactory.getOperator(mutationType);
		if (operator == null)
			return null; // no operator

		List<JsonNode> retList = new ArrayList<JsonNode>();

		try {
			for (OnElementType onElementConstraint : constraintSpecs) {

				// query element to be mutated
				VerySimpleXpathParser pathParser = new VerySimpleXpathParser(
						onElementConstraint.getXpath());
				
				List<String> pathSegments = pathParser.getFullPath();
				String principalPath = pathParser.rebuildPrincipalPath();

				if (principalPath != null) {

					if (principalPath.equals("/")) {
						// is root
						
						JsonNode mutatedNode = operator.mutate(inputDoc, onElementConstraint);
						if (mutatedNode != null) {
							retList.add(mutatedNode);
						}
						
					} else {
						
						Map<String, List<JsonNode>> objectMap = JsonUtils.mapPath2Tree(pathSegments, inputDoc);
						if (objectMap != null){
							
							String elementName = pathParser.getPrincipalPath().get(pathParser.getPrincipalPath().size() - 1);
							
							// JsonNode newRoot = inputDoc.deepCopy();
							List<JsonNode> list = objectMap.get(elementName);
	
							if (list != null && list.size() > 0) {
								for (JsonNode n : list) {
									JsonNode mutatedNode = operator.mutate(n, onElementConstraint);
									if (mutatedNode != null) {
										
										swapNodes(inputDoc, objectMap, pathParser.getPrincipalPath(), n, mutatedNode);
	
										retList.add(inputDoc.deepCopy());
	
										// put the node back
										swapNodes(inputDoc, objectMap, pathParser.getPrincipalPath(), mutatedNode, n);
									}
								}
							}
							
						}
					}
				}

			}
		} catch (PathException ex) {
			ex.printStackTrace();
		}

		return retList;
	}

	
	/**
	 * Swap a node of a tree with a new node
	 * @param rootNode
	 * @param nodeTobeReplaced
	 * @param newNode
	 */
	private void swapNodes(JsonNode root, Map<String, List<JsonNode>> objectMap,
			List<String> principalPath, JsonNode n, JsonNode mutatedNode) {
		JsonNode father = null;
		String elementName = principalPath.get(principalPath.size() - 1);
		if (principalPath.size() == 1)
			father = root;
		else {
			String fatherName = principalPath.get(principalPath.size() - 2);
			List<JsonNode> possibleFathers = objectMap.get(fatherName);
			for (JsonNode f : possibleFathers) {
				if (f.isArray()) {
					Iterator<JsonNode> iter = f.iterator();
					while (iter.hasNext()) {
						if (iter.next().equals(n)) {
							father = f;
							break;
						}
					}
					if (father != null)
						break;
				} else if (f.get(elementName).equals(n)) {
					father = f;
					break;
				}
			}
		}
		
		if (father != null){
			if (father.path(elementName).isArray()){
				ArrayNode node = (ArrayNode) father.path(elementName);
				int tobeswap = node.size();
				List<JsonNode> nodeList = new ArrayList<JsonNode>(); // used to keep node order
				for (int i = 0; i < node.size(); i++){
					nodeList.add(node.get(i));
					if (node.get(i).equals(n)){
						tobeswap = i;
					}
				}
				if (tobeswap < node.size()){
					node.removeAll();
					for (int i = 0; i < nodeList.size(); i++){
						if (i == tobeswap){
							node.add(mutatedNode); // las
						} else {
							node.add(nodeList.get(i));
						}
					}
					nodeList.clear();
					nodeList = null;
				}
			} else {
				((ObjectNode)father).put(elementName, mutatedNode);
			}
		}
		
	}

	
	/**
	 * Main method, to be use as a standalone version
	 * 
	 */
	public static void main(String[] args) {
		if (args.length != 3) {
			System.out
					.println("Usage: java JsonMutator inputConstraintFile inputJsonFile outpurDir");
			System.exit(-1);
		}

		String inputConstraint = FileUtils.checkFileDir(args[0]) == 1 ? args[0]
				: null;
		String inputJsonFile = FileUtils.checkFileDir(args[1]) == 1 ? args[1]
				: null;
		String outpurDir = FileUtils.checkFileDir(args[2]) == 0 ? args[2]
				: null;

		if (inputConstraint == null || inputJsonFile == null
				|| outpurDir == null) {
			System.out
					.println("The given paramters is not correct or doesn't exist!");
			System.out.println("\tConstraints: " + inputConstraint);
			System.out.println("\tInput Json File: " + inputJsonFile);
			System.out.println("\tOutput Dir: " + outpurDir);
			System.exit(-1);
		}

		JsonMutator mutator = new JsonMutator(inputConstraint);
		JsonNode originalDoc = FileUtils.load(new File(inputJsonFile));
		List<JsonNode> mutants = mutator.mutate(originalDoc);

		// save to dir
		if (mutants != null) {
			for (int i = 0; i < mutants.size(); i++) {
				JsonNode mutant = mutants.get(i);
				String outFileName = outpurDir + File.separatorChar + "RES-M"
						+ String.valueOf(i) + ".json";
				FileUtils.save(mutant, outFileName);
			}
		}
	}
}
