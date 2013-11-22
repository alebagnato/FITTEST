package fbk.se.services.mutation;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.TransformerException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.Constraints;
import fbk.se.services.mutation.constraints.OnElementType;
import fbk.se.services.mutation.constraints.Pattern;
import fbk.se.services.mutation.constraints.TotalDigits;
import fbk.se.services.mutation.constraints.WhiteSpace;
import fbk.se.services.mutation.operators.IMutationOperator;
import fbk.se.services.mutation.operators.MutationFactory;
import fbk.se.services.mutation.utils.FileUtils;
import fbk.se.services.mutation.utils.UniversalNamespaceCache;

/**
 * Main mutator, mutating an input XML file based on constraints specified for the 
 * XML elements in the file.
 * 
 * @author cdnguyen
 *
 */
public class Mutator {
	private static final String JAXB_CONSTRAINT_PACKAGE = "fbk.se.services.mutation.constraints";
	private String constraintsSpecFile;
	private Constraints constraints = null;
	private UniversalNamespaceCache nameContext = null;

	private Map<String, List<OnElementType>> mutationDB;

	public Mutator(String constraintFile) {
		this.constraintsSpecFile = constraintFile;
		init();
	}

	/**
	 * Initiate constraints from constraint spec. file
	 * 
	 */
	private void init() {
		try {
			JAXBContext jct = JAXBContext.newInstance(JAXB_CONSTRAINT_PACKAGE);
			Unmarshaller um = jct.createUnmarshaller();
			constraints = (Constraints) um.unmarshal(new File(constraintsSpecFile));
			mutationDB = new HashMap<String, List<OnElementType>>();

			for (String mutationType : IMutationOperator.XSD_MUTATION_TYPES) {
				List<OnElementType> list = new ArrayList<OnElementType>();
				mutationDB.put(mutationType, list);
			}

			for (OnElementType element : constraints.getOnElement()) {
				if (element.getRestriction() != null) {
					ComplexRestrictionType restriction = element.getRestriction();
					String base = restriction.getBase().getLocalPart();

					// constraint facets
					List<Object> facets = restriction.getFacets();
					for (Object o : facets) {
						List<OnElementType> list = null;
						if (o instanceof JAXBElement) {
							String constraintType = ((JAXBElement) o).getName().getLocalPart();
							list = mutationDB.get(constraintType);
							if (list != null)
								list.add(element);
							if (constraintType.equals(IMutationOperator.XSD_MUTATION_ENUMERATION)) {
								// there are usually more than one enumeration,
								// so exit this for to
								// avoid duplications
								break;
							}
						} else if (o instanceof Pattern) {
							list = mutationDB.get(IMutationOperator.XSD_MUTATION_PATTERN);
							if (list != null)
								list.add(element);
						} else if (o instanceof WhiteSpace) {
							list = mutationDB.get(IMutationOperator.XSD_MUTATION_WHITESPACE);
							if (list != null)
								list.add(element);
						} else if (o instanceof TotalDigits) {
							list = mutationDB.get(IMutationOperator.XSD_MUTATION_TOTALDIGITS);
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
	 * Mutate a DOM document, return a new one with mutated data
	 * 
	 * If mutation failed (due to constraint problem or whatever) return null
	 * 
	 * @param input
	 * @return
	 */
	public List<Document> mutate(final Document input) {
		// no constraint ready, return null
		if (constraints == null)
			return null;
		
		if (nameContext == null){
			nameContext = new UniversalNamespaceCache(input, false);
		}

		List<Document> retList = new ArrayList<Document>();
		
		for (String mutationType : IMutationOperator.XSD_MUTATION_TYPES){
			List<Document> tmp = mutate(input, mutationType);
			if (tmp != null && tmp.size() > 0){
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
	public List<Document> mutate(final Document inputDoc, String mutationType) {
		// no constraint ready, return null
		if (constraints == null)
			return null;
		
		if (nameContext == null){
			nameContext = new UniversalNamespaceCache(inputDoc, false);
		}

		List<OnElementType> constraintSpecs = mutationDB.get(mutationType);
		if (constraintSpecs == null || constraintSpecs.size() == 0) {
			return null; // no specs
		}

		IMutationOperator operator = MutationFactory.getOperator(mutationType);
		if (operator == null)
			return null; // no operator

		List<Document> retList = new ArrayList<Document>();

		try {
			Document newDoc = getACopyOf(inputDoc);
			XPath xpath = XPathFactory.newInstance().newXPath();
			xpath.setNamespaceContext(nameContext);
			for (OnElementType element : constraintSpecs) {
				// query if there's any results that can be mutated
				String path = element.getXpath();
				if (path != null && newDoc != null){
					NodeList list = (NodeList) xpath.evaluate(path, newDoc, XPathConstants.NODESET);
					if (list != null && list.getLength() > 0){
						for (int i = 0; i < list.getLength(); i++) {
							Node n = list.item(i);
		
							Node mutatedNode = operator.mutate(n, element);
							if (mutatedNode != null) {
								Node parent = n.getParentNode();
								// temporary replace
								parent.replaceChild(mutatedNode, n);
		
								// add a copy to the return list
								retList.add(getACopyOf(newDoc));
		
								// replace back so that there's only one change at a
								// time
								parent.replaceChild(n, mutatedNode);
							}
						}
						/*
					// Randomly pick only one is enough?
						Random selector = new Random();
						Node n = list.item(selector.nextInt(list.getLength()));
	
						Node mutatedNode = operator.mutate(n, element);
						if (mutatedNode != null) {
							Node parent = n.getParentNode();
							// temporary replace
							parent.replaceChild(mutatedNode, n);
	
							// add a copy to the return list
							retList.add(getACopyOf(newDoc));
	
							// replace back so that there's only one change at a
							// time
							parent.replaceChild(n, mutatedNode);
	
						}
						*/
					}
				}
				
			}
		} catch (XPathExpressionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return retList;
	}

	/**
	 * Effective way to clone DOM Document
	 * 
	 * @param input
	 * @return
	 * @throws TransformerException
	 */
	private Document getACopyOf(Document input) throws TransformerException {
//		TransformerFactory tfactory = TransformerFactory.newInstance();
//		Transformer tx = tfactory.newTransformer();
//		DOMSource source = new DOMSource(input);
//		DOMResult result = new DOMResult();
////		input.setStrictErrorChecking(false);
//		//tx.setOutputProperty(OutputKeys.STANDALONE, "yes");
//		tx.transform(source, result);
//		return (Document) result.getNode();
		Document newDoc = (Document)input.cloneNode(true);
		return  newDoc;
	}

	/**
	 * Main method, to be use as a standalone app
	 * 
	 */
	public static void main(String[] args) {
		if (args.length != 3){
			System.out.println("Usage: java fbk.se.services.mutation.Mutator inputConstraint inputXMLDoc outpurDir");
			System.exit(-1);
		}
		
		String inputConstraint = FileUtils.checkFileDir(args[0]) == 1? args[0] : null; 
		String inputXMLDoc =   FileUtils.checkFileDir(args[1]) == 1? args[1] : null; 
		String outpurDir =  FileUtils.checkFileDir(args[2]) == 0? args[2] : null;
		
		if (inputConstraint == null || inputXMLDoc == null
				|| outpurDir == null){
			System.out.println("The given paramters is not correct or doesn't exist!");
			System.out.println("Constraints: " + inputConstraint);
			System.out.println("Input DOM Document: " + inputXMLDoc);
			System.out.println("Output Dir: " + outpurDir);
			System.exit(-1);
		}
		
		Mutator mutator = new Mutator(inputConstraint);
		Document doc = FileUtils.loadXMLfileToDOMDoc(inputXMLDoc);
		List<Document> mutants = mutator.mutate(doc);

		// save to output dir
		if (mutants != null){
			for (int i = 0; i < mutants.size(); i++){
				Document mutant = mutants.get(i);
				String outFileName = outpurDir + File.separatorChar + "RES-M" + String.valueOf(i) + ".xml";
				FileUtils.saveDOMDocToFile(mutant, outFileName);
			}
		}
	}
}
