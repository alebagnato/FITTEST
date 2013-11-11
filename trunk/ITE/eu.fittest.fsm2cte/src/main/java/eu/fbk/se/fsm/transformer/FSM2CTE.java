package eu.fbk.se.fsm.transformer;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import javax.xml.bind.JAXBElement;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.Node;
import eu.fbk.se.fsm.cte.Class;
import eu.fbk.se.fsm.cte.Classification;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.CteObject;
import eu.fbk.se.fsm.cte.Mark.Tag;
import eu.fbk.se.fsm.cte.Mark.Tag.Content;
import eu.fbk.se.fsm.cte.TestGroup;
import eu.fbk.se.fsm.cte.CteObject.Tree;
import eu.fbk.se.fsm.tcgenerator.IAbstractTestCaseGenerator;
import eu.fbk.se.fsm.tcgenerator.PairWiseGenerator;
import eu.fbk.se.fsm.utils.JAXBUtil;
import eu.fbk.se.fsm.visitor.IFSMVisitor;
import eu.fbk.se.fsm.visitor.Path;
import eu.fbk.se.fsm.visitor.VisitorFactory;
import eu.fbk.se.fsm.xinput.AtomicParam;
import eu.fbk.se.fsm.xinput.AtomicParamRef;
import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.CompositeParam;
import eu.fbk.se.fsm.xinput.CompositeParamRef;
import eu.fbk.se.fsm.xinput.DomainInputs;
import eu.fbk.se.fsm.xinput.Event;
import eu.fbk.se.fsm.xinput.NoFixedFacet;
import eu.fbk.se.fsm.xinput.Xinput;

public class FSM2CTE {

	private int idIndex = 0;
	private String visitorName = VisitorFactory.VISITOR_BREADTHFIRST;
	
	List<IProgressListener> listenerList = new ArrayList<IProgressListener>();

	/**
	 * Register progress listener
	 * @param listener
	 */
	public void register(IProgressListener listener){
		listenerList.add(listener);
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {

		if (args.length == 2 || args.length == 3){
			// ok, go ahead
		} else {
			System.err.println("Usage java eu.fbk.se.fsm.transformer.FSM2CTE params");
			System.exit(1);
		}
		
		// TODO check if other params valid
		
		FSM2CTE transformer = new FSM2CTE();
		if (args.length == 3){
			transformer.transform(args[0], args[1], args[2], true, null);
		} else if (args.length == 4){
			transformer.transform(args[0], args[1], args[2], true, args[3]);
		}
			
	}
	
	

	public FSM2CTE(String visitorName) {
		this.visitorName = visitorName;
	}

	public FSM2CTE() {
		visitorName = VisitorFactory.VISITOR_BREADTHFIRST;
	}

	/**
	 * Read FSM Model and create CTE files for paths, regardless input domain
	 * 
	 * @author cunduy
	 * 
	 * @param inputModel
	 * @param outputFolder
	 * 
	 */
	public void transform(String inputModel, String outputFolder, String folderTraces) {
		
		// check output folder
		if (!prepareOutputFolder(outputFolder)){
			return;
		}
		
		// Parse the FSM input and generate paths
		FSM fsm = new FSM(inputModel);
		IFSMVisitor bfv = VisitorFactory.getVisitor(visitorName,inputModel,folderTraces);
		bfv.visit(fsm);

		Vector<Path> paths = bfv.getPaths();
		
		if (paths == null || paths.size() == 0)
			return;
		
		Node startNode = fsm.getStartNode();
		int i = 0;

		// Notify listeners
		for (IProgressListener listener : listenerList){
			listener.start(paths.size());
		}
				
		for (Path path : paths) {

			CteObject cte = new CteObject();
			String ctePathName = "p" + String.valueOf(i);
			resetIdIndex();
			cte.setId(genId());

			// create tree
			Tree tree = new Tree();
			tree.setId(genId());
			tree.setName(ctePathName + "_tree");
			cte.setTree(tree);

			// create root of the tree
			Composition composition = new Composition();
			composition.setId(genId());
			composition.setName(ctePathName);
			tree.setRoot(composition);
			tree.getCompositionOrClassificationOrClazz().add(composition);

			// create a sequence of compositions corresponding to generated path
			Composition sequenceComposition = new Composition();
			sequenceComposition.setId(genId());
			sequenceComposition.setName(ctePathName + "_sequence");
			composition.getCompositionOrClassification().add(
					sequenceComposition);

			String stateLabel = startNode.getLabel();
			for (Edge e : path.getEdges()) {

				String cteNodeName = stateLabel + "_" + e.getEvent();

				Composition nodeComposition = new Composition();
				nodeComposition.setId(genId());
				nodeComposition.setName(cteNodeName);
				sequenceComposition.getCompositionOrClassification().add(
						nodeComposition);

				stateLabel = e.getTarget().getLabel();
			}

			// Add an empty test group
			TestGroup tg = new TestGroup();
			tg.setId(genId());
			tg.setName("TG");
			cte.setTestGroup(tg);

			// Save file to output folder
			String outFile = outputFolder + File.separatorChar + ctePathName
					+ ".cte";
			JAXBUtil.saveCte(cte, outFile);

			i++;
			
			// Notify listeners
			for (IProgressListener listener : listenerList){
				listener.progress(1);
			}
		}
		
		// Notify listeners
		for (IProgressListener listener : listenerList){
			listener.finish();
		}

	}

	/**
	 * Read FSM Model and create CTE files for paths, CTE is detailed with input
	 * domain specification following xinput.xsd format
	 * 
	 * @author cunduy
	 * 
	 * @param inputDomain
	 * @param inputModel
	 * @param outputFolder
	 * @param folderTraces
	 */
	public void transform(String domainInput, String inputModel,
			String outputFolder, boolean generateTestCase, String folderTraces) {
		
		// check output folder
		if (!prepareOutputFolder(outputFolder)){
			return;
		}
		
		// Unmarshal domainInput xml file
		DomainInputs domainInputs = JAXBUtil.loadDomainInputs(domainInput);
		if (domainInputs == null) {
			return;
		}

		// build a hashmap to accelerate the access to inputs
		HashMap<String, Xinput> inputMap = new HashMap<String, Xinput>();
		for (Xinput input : domainInputs.getXinput()) {
			if (input.getEvents() != null) {
				for (Object event : input.getEvents()) {
					inputMap.put(((Event)event).getId(), input);
				}
			}
		}
		
		// For Debugging
//		for (String key : inputMap.keySet()){
//			System.out.println(key);
//		}
		
		// Initiate the test case generator
		IAbstractTestCaseGenerator tcGenerator = new PairWiseGenerator();

		// Parse the FSM input and generate paths
		FSM fsm = new FSM(inputModel);
		IFSMVisitor bfv = VisitorFactory.getVisitor(visitorName,inputModel,folderTraces);
//		BreadthFirstVisit bfv = new BreadthFirstVisit();
		bfv.visit(fsm);

		Vector<Path> paths = bfv.getPaths();
		
		if (paths == null || paths.size() == 0)
			return;
		
//		Vector<Vector<Edge>> paths = bfv.getPaths();
		Node startNode = fsm.getStartNode();
		int i = 0;
		
		// Notify listeners
		for (IProgressListener listener : listenerList){
			listener.start(paths.size());
		}

		for (Path path : paths) {
//		for (Vector<Edge> path : paths) {

			CteObject cte = new CteObject();
			String ctePathName = "p" + String.valueOf(i);
			resetIdIndex();
			cte.setId(genId());

			// create tree
			Tree tree = new Tree();
			tree.setId(genId());
			tree.setName(ctePathName + "_tree");
			cte.setTree(tree);

			// create root of the tree
			Composition composition = new Composition();
			composition.setId(genId());
			composition.setName(ctePathName);
			tree.setRoot(composition);
			tree.getCompositionOrClassificationOrClazz().add(composition);

			// create a sequence of compositions corresponding to generated path
			Composition sequenceComposition = new Composition();
			sequenceComposition.setId(genId());
			sequenceComposition.setName(ctePathName + "_sequence");
			composition.getCompositionOrClassification().add(
					sequenceComposition);

			String stateLabel = startNode.getLabel();
			for (Edge e : path.getEdges()) {
//			for (Edge e : path) {

				String cteNodeName = stateLabel + "_" + e.getEvent();

				Composition nodeComposition = new Composition();
				nodeComposition.setId(genId());
				nodeComposition.setName(cteNodeName);
				sequenceComposition.getCompositionOrClassification().add(
						nodeComposition);

				// populate detailed inputs from the domain inputs for the
				// current node
				Xinput detailedInput = inputMap.get(e.getEvent());
				
//				System.out.println(e.getEvent());
//				if (inputMap.keySet().contains(e.getEvent())){
//					System.out.println("Contained!");
//				}
				
				if (detailedInput != null) {
					populateNodeInputs(nodeComposition, detailedInput);
				}

				stateLabel = e.getTarget().getLabel();
			}

			// Add an empty test group
			TestGroup tg = new TestGroup();
			tg.setId(genId());
			tg.setName("TG");
			cte.setTestGroup(tg);
			
			// Generate abstract test cases if specified
			if (generateTestCase){
				tcGenerator.generateTestCases(cte);
			}

			// Save file to output folder
			String outFile = outputFolder + File.separatorChar + ctePathName
					+ ".cte";
			JAXBUtil.saveCte(cte, outFile);

			i++;
			
			// Notify listeners
			for (IProgressListener listener : listenerList){
				listener.progress(1);
			}
		}

		
		// Notify listeners
		for (IProgressListener listener : listenerList){
			listener.finish();
		}
	}

	
	public void transform(String domainInput, String inputModel,
			String outputFolder, boolean generateTestCase, boolean optimizeGeneratedTrees, String folderTraces) {
		
		transform(domainInput, inputModel, outputFolder, generateTestCase, folderTraces);

		// optimize only when there are test cases
		if (generateTestCase && optimizeGeneratedTrees){
			String optimizedFolder;
			if (!outputFolder.endsWith(File.separator)) 
				optimizedFolder	= outputFolder + "-optimized";
			else {
				optimizedFolder	= outputFolder.substring(0, outputFolder.length() - 1) + "-optimized";
			}
			
			PostOptimizer optimizer = new PostOptimizer();
			optimizer.optimize(outputFolder, optimizedFolder);
		}
		
	}
	
	/**
	 * Populate the detailed input for the tree node
	 * 
	 * @author cunduy
	 * 
	 * @param nodeComposition
	 * @param detailedInput
	 */
	private void populateNodeInputs(Composition nodeComposition,
			Xinput detailedInput) {

		// combine atomic params
		List<AtomicParam> atomicParams = new ArrayList<AtomicParam>();
		if (detailedInput.getAtomicParam().size() > 0
				|| detailedInput.getAtomicParamRef().size() > 0) {
			for (AtomicParam param : detailedInput.getAtomicParam()) {
				atomicParams.add(param);
			}

			for (AtomicParamRef paramRef : detailedInput.getAtomicParamRef()) {
				try {
					AtomicParam param = (AtomicParam) paramRef.getParamRef();
					atomicParams.add(param);
				} catch (Exception e) {
					// user specifies wrong reference, skip
					// TODO: handle exception
				}
			}
		}

		// generate subtree for each atomic param
		for (AtomicParam param : atomicParams) {
			Classification paramClassification = createClassification(param);
			nodeComposition.getCompositionOrClassification().add(
					paramClassification);
		}

		// combine composite params
		List<CompositeParam> compositeParams = new ArrayList<CompositeParam>();
		if (detailedInput.getCompositeParam().size() > 0
				|| detailedInput.getCompositeParamRef().size() > 0) {
			for (CompositeParam param : detailedInput.getCompositeParam()) {
				compositeParams.add(param);
			}

			for (CompositeParamRef paramRef : detailedInput
					.getCompositeParamRef()) {
				try {
					CompositeParam param = (CompositeParam) paramRef
							.getParamRef();
					compositeParams.add(param);
				} catch (Exception e) {
					// TODO: handle exception
				}
			}
		}

		// generate subtree for the composite param
		for (CompositeParam param : compositeParams) {
			Composition paramComposition = createComposition(param);
			nodeComposition.getCompositionOrClassification().add(
					paramComposition);
		}

	}

	/**
	 * Create a CTE composition for a composite parameter
	 * 
	 * @author cunduy
	 * 
	 * @param param
	 * @return
	 */
	private Composition createComposition(CompositeParam param) {

		Composition composition = new Composition();
		composition.setId(genId());
		
		if (param.getName() != null && param.getName().length() > 0)
			composition.setName(param.getName());
		else
			composition.setName(param.getId());

		// combine atomic params
		List<AtomicParam> atomicParams = new ArrayList<AtomicParam>();
		if (param.getAtomicParam().size() > 0
				|| param.getAtomicParamRef().size() > 0) {
			for (AtomicParam p : param.getAtomicParam()) {
				atomicParams.add(p);
			}

			for (AtomicParamRef paramRef : param.getAtomicParamRef()) {
				try {
					AtomicParam p = (AtomicParam) paramRef.getParamRef();
					atomicParams.add(p);
				} catch (Exception e) {
					// user specifies wrong reference, skip
					// TODO: handle exception
				}
			}
		}

		for (AtomicParam aParam : atomicParams) {
			Classification classification = createClassification(aParam);
			composition.getCompositionOrClassification().add(classification);
		}

		// combine composite params
		List<CompositeParam> compositeParams = new ArrayList<CompositeParam>();
		if (param.getCompositeParam().size() > 0
				|| param.getCompositeParamRef().size() > 0) {
			for (CompositeParam p : param.getCompositeParam()) {
				compositeParams.add(p);
			}

			for (CompositeParamRef paramRef : param.getCompositeParamRef()) {
				try {
					CompositeParam p = (CompositeParam) paramRef.getParamRef();
					compositeParams.add(p);
				} catch (Exception e) {
					// TODO: handle exception
				}
			}
		}

		// generate subtree for the composite param
		for (CompositeParam p : compositeParams) {
			// create recursively composite params
			Composition paramComposition = createComposition(p);
			composition.getCompositionOrClassification().add(paramComposition);
		}

		return composition;
	}

	/**
	 * Create a CTE classification for an atomic parameter
	 * 
	 * @author cunduy
	 * 
	 * @param param
	 * @return
	 */
	private Classification createClassification(AtomicParam param) {
		Classification clz = new Classification();
		clz.setId(genId());
		if (param.getName() != null && param.getName().length() > 0)
			clz.setName(param.getName());
		else
			clz.setName(param.getId());
		
		if (param.getDataClz().size() == 0)
			return clz;
		
		double uniformOccurenceProb = 1.0 / param.getDataClz().size();

		for (ComplexDataSpecType dataClz : param.getDataClz()) {
			// check if dataClz is an enumeration
			if (!isEnumeration(dataClz)){
				
				Class cteClass = new Class();
				cteClass.setId(genId());
				if (dataClz.getName() != null && dataClz.getName().length() > 0)
					cteClass.setName(dataClz.getName());
				else 
					cteClass.setName(dataClz.getId());
				
				Tag tag = createTag(uniformOccurenceProb);
				cteClass.getTagGroup().add(tag);
				
				clz.getClazz().add(cteClass);
			} else {
				
				List<Object> facets = dataClz.getFacets();
				
				double elementUniformOccurenceProb = uniformOccurenceProb / dataClz.getFacets().size();
				
				for (Object o : facets) {
					if (o instanceof JAXBElement) {
						JAXBElement tmp = (JAXBElement) o;
						NoFixedFacet valueFacet = (NoFixedFacet) tmp.getValue();
						String value = valueFacet.getValue();
						
						Class cteClass = new Class();
						cteClass.setId(genId());
						cteClass.setName(value);
						Tag tag = createTag(elementUniformOccurenceProb);
						cteClass.getTagGroup().add(tag);
						
						clz.getClazz().add(cteClass);
					}
				}
			}
		}

		return clz;
	}
	
	
	
	private Tag createTag(double occurenceProb) {
		Tag tag = new Tag();
		tag.setId(genId());
		tag.setType("PriorityGen");
		
		Content error = new Content();
		error.setKey("error"); error.setValue("-1.0");
		tag.getContent().add(error);

		Content occurrence = new Content();
		occurrence.setKey("occurrence"); occurrence.setValue(String.valueOf(occurenceProb));
		tag.getContent().add(occurrence);
		
		Content cost = new Content();
		cost.setKey("cost"); cost.setValue("-1.0");
		tag.getContent().add(cost);
		
		return tag;
	}



	/**
	 * Check if data class is an enumeration
	 * @param dataClz
	 * @return
	 */
	private boolean isEnumeration(ComplexDataSpecType dataClz) {
		List<Object> facets = dataClz.getFacets();
		for (Object o : facets) {
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement) o;
				String facetType = tmp.getName().getLocalPart();
				if (facetType.equals("enumeration")){
					return true;
				}
			}
		}
		return false;
	}



	/**
	 * Generate new Id based in current idIndex value
	 * 
	 * @author cunduy
	 * 
	 * @return
	 */
	private String genId() {
		idIndex++;
		return "c" + String.valueOf(idIndex);
	}

	/**
	 * Reset idIndex to zero
	 * 
	 * @author cunduy
	 * 
	 */
	private void resetIdIndex() {
		idIndex = 0;
	}
	
	/**
	 * Check and prepare output folder
	 * @param folderName
	 * @return
	 */
	private boolean prepareOutputFolder(String folderName){
		File outFolder = new File(folderName);
		try {
			outFolder.mkdirs();
			return true;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

}
