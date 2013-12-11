/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.xinputmining;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import com.stromberglabs.cluster.Cluster;
import com.stromberglabs.cluster.ClusterUtils;
import com.stromberglabs.cluster.KClusterer;
import com.stromberglabs.cluster.KMeansClusterer;
import com.stromberglabs.cluster.UniPoint;

import eu.fbk.se.fsm.Edge;
import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.Node;
import eu.fbk.se.fsm.utils.JAXBUtil;
import eu.fbk.se.fsm.xinput.AtomicParam;
import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.DomainInputs;
import eu.fbk.se.fsm.xinput.Event;
import eu.fbk.se.fsm.xinput.Facet;
import eu.fbk.se.fsm.xinput.NoFixedFacet;
import eu.fbk.se.fsm.xinput.NumFacet;
import eu.fbk.se.fsm.xinput.WebElementType;
import eu.fbk.se.fsm.xinput.Xinput;
import eu.fbk.xinputmining.logdata.LogData;
import eu.fbk.xinputmining.logdata.StringData;
import eu.fittest.itelog.Body;

public class XinputMiner {
	private static final int  MIN_CLASS_NUMBER = 2;
	private static final int  MAX_CLASS_NUMBER = 4;
	
	public boolean mine(String inputModel, String inputTraceFolder,
			String outputXML) {
		DomainInputs domainInput = mine(inputModel, outputXML);
		if (domainInput != null) {
			// mine domain input classifications
			List<Body> logData = null;
			if (inputTraceFolder != null){
				logData = XMLUtils.loadXMLLog(inputTraceFolder, 1000);
			}
			mineClassifications(logData, domainInput);
			
			JAXBUtil.saveDomainInputs(domainInput, outputXML);
			return true;
		} else {
			return false;
		}

	}

	/**
	 * No log is available
	 * @param domainInput
	 */
	private void  mineClassifications(List<Body> logData, DomainInputs domainInput) {
		List<Xinput> xinputList = domainInput.getXinput(); 
		List<Xinput> newXinputList = new ArrayList<Xinput>();
		for (Event event : domainInput.getEvent()){
			if (event.getWebType() != null){
				if (event.getWebType().equals(WebElementType.INPUT)
						|| event.getWebType().equals(WebElementType.SELECT)){
					// check if the corresponding xinput exists 
					boolean alreadySpecified = false;
					for (Xinput xinput : xinputList){
						if (xinput.getEvents().contains(event)){
							// xinput exist, skipping
							alreadySpecified = true;
							break;
						}
					}
					if (!alreadySpecified){
						Xinput newXinputSpec = new Xinput();
						String eventName = getEventName(event);
						newXinputSpec.setId(eventName + "_input");
						newXinputSpec.getEvents().add(event); 
						
						AtomicParam defaultParam = new AtomicParam();
						defaultParam.setId(eventName + "_param1");
										
						ComplexDataSpecType clz1 = null;
						if (logData == null || logData.size() == 0){
							clz1 = generateDefaultEnum(eventName);
							defaultParam.getDataClz().add(clz1);
						} else {
							List<ComplexDataSpecType> clzs = mineClasses(event, eventName, logData);
							for (ComplexDataSpecType clz : clzs){
								defaultParam.getDataClz().add(clz);
							}
						}
						
						newXinputSpec.getAtomicParam().add(defaultParam);
						newXinputList.add(newXinputSpec);
					}
				}
			}
		}
		if (newXinputList.size() > 0)
			domainInput.getXinput().addAll(newXinputList);
	}

	/**
	 * Mine input classifications for the event. 
	 * @param event
	 * @param eventName
	 * @param logData
	 * @return
	 */
	private List<ComplexDataSpecType> mineClasses(Event event, String eventName,
			List<Body> allLogData) {
		String queryType = null;
		String queryValue = null;
		
		List<ComplexDataSpecType> clzs = new ArrayList<ComplexDataSpecType>();
		
		if (event.getReachedById() != null){
			queryType = "id";
			queryValue = event.getReachedById();
		} else if (event.getReachedByName() != null){
			queryType = "name";
			queryValue = event.getReachedByName();
		} else if (event.getReachedByCSS() != null){
			queryType = "css";
			queryValue = event.getReachedByCSS();
		} else if (event.getReachedByXPath() != null){
			queryType = "xpath";
			queryValue = event.getReachedByXPath();
		}
		
		if (queryType != null && queryValue != null){
			try {
				LogData data = XMLUtils.aggregateLogData(queryType, queryValue, allLogData);
				if (data != null){
					if (data.shouldBeEnumerated()){
						ComplexDataSpecType clz1 = new ComplexDataSpecType();
						clz1.setName(eventName + "_mined_clz");
						clz1.setBase(new QName("http://www.fbk.eu/xinput", data.getDataType()));
						
						for (Object entry : data.getEntries()){
							NoFixedFacet enumvalue = new NoFixedFacet();
							enumvalue.setValue(data.format(entry));
							QName qname = new QName("http://www.fbk.eu/xinput", "enumeration");
							JAXBElement<NoFixedFacet> enum1 = new JAXBElement<NoFixedFacet>(qname, NoFixedFacet.class, enumvalue);
							clz1.getFacets().add(enum1);
						}
						
						clzs.add(clz1);
					} else if (data.shouldUseStringBoundary()){
						int maxLen = ((StringData)data ).getMaxLength();
						int minLen = ((StringData)data ).getMinLength();
						
						ComplexDataSpecType clz1 = new ComplexDataSpecType();
						clz1.setName(eventName + "_mined_clz");
						clz1.setBase(new QName("http://www.fbk.eu/xinput", "string"));
						
						NumFacet minEnumvalue = new NumFacet();
						minEnumvalue.setValue(String.valueOf(minLen));
						QName qname = new QName("http://www.fbk.eu/xinput", "minLength");
						JAXBElement<NumFacet> minEnum = new JAXBElement<NumFacet>(qname, NumFacet.class, minEnumvalue);
						clz1.getFacets().add(minEnum);

						NumFacet maxEnumvalue = new NumFacet();
						maxEnumvalue.setValue(String.valueOf(maxLen));
						qname = new QName("http://www.fbk.eu/xinput", "maxLength");
						JAXBElement<NumFacet> maxEnum = new JAXBElement<NumFacet>(qname, NumFacet.class, maxEnumvalue);
						clz1.getFacets().add(maxEnum);
						
						clzs.add(clz1);
						
					} else if (data.shouldBeClustered()){
						
						List<UniPoint> allPoints = data.getEntries(UniPoint.class);
						
						int size = (int) Math.sqrt((double)allPoints.size() / 2.0);
						
						// Consider only 2 -> 4 classes 
						if (size < MIN_CLASS_NUMBER) size = MIN_CLASS_NUMBER;
						if (size > MAX_CLASS_NUMBER) size = MAX_CLASS_NUMBER;
						
						// Do clustering
						KClusterer clusterer = new KMeansClusterer();
						Cluster[] clusters = clusterer.cluster(allPoints,size);
						
						int clzCounter = 1;
						for ( Cluster c : clusters ){
							String clzName = eventName + "_mined_clz_" + String.valueOf(clzCounter++);
							
							ComplexDataSpecType clz = new ComplexDataSpecType();
							clz.setName(clzName);
							clz.setBase(new QName("http://www.fbk.eu/xinput", data.getDataType()));
							
							Facet minEnumvalue = new Facet();
							minEnumvalue.setValue(data.format(ClusterUtils.getMin(c)));
							QName qname = new QName("http://www.fbk.eu/xinput", "minInclusive");
							JAXBElement<Facet> minEnum = new JAXBElement<Facet>(qname, Facet.class, minEnumvalue);
							clz.getFacets().add(minEnum);

							Facet maxEnumvalue = new Facet();
							maxEnumvalue.setValue(data.format(ClusterUtils.getMax(c)));
							qname = new QName("http://www.fbk.eu/xinput", "maxInclusive");
							JAXBElement<Facet> maxEnum = new JAXBElement<Facet>(qname, Facet.class, maxEnumvalue);
							clz.getFacets().add(maxEnum);
							
							clzs.add(clz);
							
						}
					}
					
				} else {
					ComplexDataSpecType clz1 = generateDefaultEnum(eventName);
					clzs.add(clz1);
				}
				
			} catch (MiningException e) {
				System.err.println(e.getMessage());
			}
		}
		
		return clzs;
	}

	/**
	 * Generate a default input, user has to specify manually. This method will create a template any way 
	 * so that the user can start quickly.
	 * 
	 * @param eventName
	 * @return
	 */
	private ComplexDataSpecType generateDefaultEnum(String eventName) {
		ComplexDataSpecType clz1 = new ComplexDataSpecType();
		clz1.setName(eventName + "_default_clz");
		clz1.setBase(new QName("http://www.fbk.eu/xinput", "string"));
		
		NoFixedFacet enumvalue = new NoFixedFacet();
		enumvalue.setValue("to-be-defined");
		QName qname = new QName("http://www.fbk.eu/xinput", "enumeration");
		JAXBElement<NoFixedFacet> enum1 = new JAXBElement<NoFixedFacet>(qname, NoFixedFacet.class, enumvalue);
		clz1.getFacets().add(enum1);
		
		return clz1;
	}

	/**
	 * Get a name for the event from different sources
	 * @param event
	 * @return
	 */
	private String getEventName(Event event) {
		if (event.getReachedByName() != null)
			return event.getReachedByName();

		if (event.getReachedById() != null)
			return event.getReachedById();
		
		return event.getId();
	}

	

	/**
	 * Mining an xinput file from a FSM model, or at least prepare a skeleton of
	 * an xinput file for the events specified in the FSM model. If the
	 * outputXML exist, the method will update the file with new events.
	 * 
	 * @param inputModel
	 * @param outputXML
	 * @return
	 */
	private DomainInputs mine(String inputModel, String outputXML) {

		FSM fsm = new FSM(inputModel);

		String name = "mined-xinput-for";
		int start = inputModel.lastIndexOf(File.separatorChar);
		int end = inputModel.indexOf(".fsm");
		if (start < end && start > 0) {
			name = name + "-" + inputModel.substring(start + 1, end);
		}

		DomainInputs domainInput = null;
		File output = new File(outputXML);
		if (output.exists()) {
			domainInput = JAXBUtil.loadDomainInputs(outputXML);
		}
		if (domainInput == null) {
			domainInput = new DomainInputs();
			domainInput.setName(name);
			domainInput.setVersion("initial");
		}

		List<String> eventLabels = new ArrayList<String>();
		List<Event> eventList = domainInput.getEvent();
		if (eventList.size() > 0) {
			for (Event e : eventList) {
				eventLabels.add(e.getId());
			}
		}

		for (Node node : fsm.getNodes()) {
			for (Edge e : node.getSucc()) {
				String eLabel = e.getEvent();
				if (!eventLabels.contains(eLabel)) {
					eventLabels.add(eLabel);
					Event event = generateEvent(eLabel);
					domainInput.getEvent().add(event);
				}
			}
		}

		eventLabels.clear();
		eventLabels = null;

		return domainInput;
	}

	/**
	 * 
	 * @param eventSpec
	 * @return
	 */
	private Event generateEvent(String eventSpec) {

		Event event = new Event();

		event.setId(eventSpec);

		// fittest php patterns
		String phpPattern = "\\s*([a-zA-Z0-9]+)\\{(.*)\\}\\s*";

		Pattern p = Pattern.compile(phpPattern);
		Matcher m = p.matcher(eventSpec);
		if (m.matches()) {
			String webEvent = m.group(1);

			if ("open".equals(webEvent)) {
				event.setWebType(WebElementType.LINK);
				event.setTargetEventToFire("open");

			} else if ("type".equals(webEvent)) {
				event.setWebType(WebElementType.INPUT);
				event.setTargetEventToFire("type");
			} else if ("submit".equals(webEvent)) {
				event.setWebType(WebElementType.BUTTON);
				event.setTargetEventToFire("click");
			} else if (webEvent.startsWith("clicklink")) {
				event.setWebType(WebElementType.LINK);
				event.setTargetEventToFire("click");
			} else if ("select".equals(webEvent)) {
				event.setWebType(WebElementType.SELECT);
				event.setTargetEventToFire("select");
			} else if (webEvent.startsWith("click")) {
				event.setTargetEventToFire("click");
			}

			String detailedEvent = m.group(2);
			String[] elements = detailedEvent.split(", ");
			if (elements != null && elements.length > 0) {
				for (String element : elements) {
					if (element.startsWith("id=")) {
						event.setReachedById(element.substring(3));
					} else if (element.startsWith("xpath=")) {
						event.setReachedByXPath(element.substring(6));
					} else if (element.startsWith("name=")) {
						event.setReachedByName(element.substring(5));
					} else if (element.startsWith("css=")) {
						event.setReachedByCSS(element.substring(4));
					} else if (element.startsWith("link=")) {
						event.setReachedByLinkText(element.substring(5));
					} else if (element.startsWith("url=")) {
						event.setReachedByURL(element.substring(4));
					} else {
						event.setTargetEventToFire("eventSpec");
					}
				}
			}
		} else {
			if (eventSpec.startsWith("type")) {
				event.setWebType(WebElementType.INPUT);
				event.setTargetEventToFire("type");
			} else if (eventSpec.startsWith("submit")
					|| eventSpec.startsWith("click")) {
				event.setWebType(WebElementType.BUTTON);
				event.setTargetEventToFire("click");
			} else if (eventSpec.startsWith("clicklink")) {
				event.setWebType(WebElementType.LINK);
				event.setTargetEventToFire("click");
			} else if (eventSpec.startsWith("select")) {
				event.setWebType(WebElementType.SELECT);
				event.setTargetEventToFire("select");
			}

			event.setReachedByName(eventSpec);
		}

		return event;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

//		String s = "input[type=\"submit\"]";
//		System.out.println(s.replaceAll("\"", "\\\\\""));

		 if (args.length != 3){
			 System.out.println("Usage: XinputMiner inputFsmModel, logFolder, outputXML");
			 return;
		 }
		 
		//
		 
		 String inputModel = args[0]; //  "/Users/cdnguyen/Documents/research/FITTEST/fittest-svn/Software/FBK/workspace/eu.fittest.selenium2fsm/src/test/resources/output/CuteNews.fsm";
		 String logFolder = args[1]; //  "/Users/cdnguyen/Documents/research/FITTEST/fittest-svn/Software/FBK/workspace/eu.fittest.selenium2fsm/src/test/resources/output/tmp";
		String xinputFile = args[2]; // "/Users/cdnguyen/Documents/research/FITTEST/fittest-svn/Software/FBK/workspace/eu.fittest.selenium2fsm/src/test/resources/output/CuteNews.xml";
		XinputMiner miner = new XinputMiner();
		miner.mine(inputModel, logFolder, xinputFile);
	}

}
