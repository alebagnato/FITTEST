package eu.fbk.xinputmining;


import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.Binder;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import eu.fbk.xinputmining.logdata.LogData;
import eu.fbk.xinputmining.logdata.LogDataFactory;
import eu.fittest.itelog.Body;
import eu.fittest.itelog.OType;
import eu.fittest.itelog.OType.Fd;

public class XMLUtils {
	
	/**
	 * Aggregate all logged data
	 * 
	 * @param type
	 * @param value
	 * @param inputs
	 * @return
	 * @throws MiningException
	 */
	public static LogData aggregateLogData(String type, String value, List<Body> inputs) throws MiningException{
		List<OType> allData = queryAllByType(type, value, inputs);
		int dataTypeCounts[] = new int[LogData.ALL_DATA_TYPES.length];
		
		// read data type from log
		for (OType entry : allData){
			if (entry.getFd().size() > 0){
				Fd dataEntry = entry.getFd().get(entry.getFd().size() - 1);
				if (dataEntry.getV() != null){
					for (int i = 0; i < LogData.ALL_DATA_TYPES.length; i++){
						if (dataEntry.getV().getTy().equalsIgnoreCase(LogData.ALL_DATA_TYPES[i])){
							dataTypeCounts[i]++;
							break;
						}
					}
				}
			}
		}
		String logDataType = null;
		int maxCount = Integer.MIN_VALUE;
		int maxIndex = 0; // default
		for (int i = 0; i < LogData.ALL_DATA_TYPES.length; i++){
			if (LogData.ALL_DATA_TYPES[i].equals(LogData.DATA_TYPE_STRING)
					&&  dataTypeCounts[i] > 0){
				logDataType = LogData.DATA_TYPE_STRING;
				break;
			} else if (dataTypeCounts[i] > 0) {
				if (dataTypeCounts[i] > maxCount){
					maxIndex = i;
					maxCount = dataTypeCounts[i];
				}
			}
		}
		
		if (logDataType == null){
			logDataType = LogData.ALL_DATA_TYPES[maxIndex];
		}
		
		LogData data = LogDataFactory.createLogData(logDataType);
		
		for (OType entry : allData){
			if (entry.getFd().size() > 0){
				Fd dataEntry = entry.getFd().get(entry.getFd().size() - 1);
				if (dataEntry.getV() != null){
					if (dataEntry.getV().getV() != null){
						data.add(dataEntry.getV().getV());
					}
				}
			}
		}
		
		return data;
	}
	
	/*
	 * 
	 */
	public static  List<OType> queryAllByType(String type, String value, List<Body> inputs){
		List<OType> retList = new ArrayList<OType>();
		for (Body body : inputs){
			List<OType> queryResult = queryByType(type, value, body);
			if (queryResult != null){
				retList.addAll(queryResult);
			}
		}
		
		return retList;
		
	}
	
	/**
	 * XQuery the data elements for the type/value specified
	 * 
	 * 
	 * @author cdnguyen
	 * @param type
	 *            type can be id, name, xpath, etc.
	 * @param value
	 *            value of the id, name or xpath to be find
	 * @param input
	 *            input is a log body
	 * @return
	 */
	public static List<OType> queryByType(String type, String value, Body input) {
		if (input == null || value == null)
			return null;

		Document inputDom = convertToDOM(input);
		if (inputDom == null)
			return null;

		XPath xpath = XPathFactory.newInstance().newXPath();
//		xpath.setNamespaceContext(new NamespaceContext() {
//
//			public Iterator getPrefixes(String namespaceURI) {
//				return null;
//			}
//
//			public String getPrefix(String namespaceURI) {
//				return "tns";
//			}
//
//			public String getNamespaceURI(String prefix) {
////				return "http://www.fittest.eu/itelog";
//				return null;
//			}
//		});

		String path = "//fd[@n=\"" + type + "\"]/V[@v=\"" + value
				+ "\"]";

		try {
			XPathExpression expr = xpath.compile(path);
			NodeList resNode = (NodeList) expr.evaluate(inputDom,
					XPathConstants.NODESET);
			if (resNode != null) {
				List<OType> retList = new ArrayList<OType>();
				for (int i = 0; i < resNode.getLength(); i++) {
					Node n = resNode.item(i);
					Object obj = w3cNode2Object(n.getParentNode().getParentNode());
					// must be an OType
					if (obj instanceof OType) {
						retList.add((OType) obj);
					} else if (obj instanceof JAXBElement){
						Object o = ((JAXBElement) obj).getValue();
						if (o instanceof OType){
							retList.add((OType) o);
						}
					}
				}

				return retList;
			}

		} catch (XPathExpressionException e) {
			e.printStackTrace();
		}
		return null;

	}

	/**
	 * Convert JAXB object to DOM for xpath queries
	 * 
	 * @author cunduy
	 */
	private static DocumentBuilder builder = null;
	private static JAXBContext jcontext = null;

	public static Document convertToDOM(Body input) {

		try {

			if (builder == null) {
				DocumentBuilderFactory fact = DocumentBuilderFactory
						.newInstance();
				fact.setNamespaceAware(true);
				builder = fact.newDocumentBuilder();
			}

			Document doc = builder.newDocument();

			// bind the doc with input
//			QName qname = new QName("http://www.fittest.eu/itelog", "body");
			QName qname = new QName(null, "body");

			if (jcontext == null)
				jcontext = JAXBContext.newInstance("eu.fittest.itelog");
			Binder<Node> binder = jcontext.createBinder();
			binder.marshal(new JAXBElement<Body>(qname, Body.class, input), doc);

			return doc;
		} catch (Exception e) {
			// TODO: handle exception
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Convert W3C Node to JAXB object
	 * 
	 * @param node
	 * @return
	 */
	public static Object w3cNode2Object(Node node) {
		try {
//			if (jcontext == null)
			JAXBContext	jcontext = JAXBContext.newInstance("eu.fittest.itelog");
			Binder<Node> binder = jcontext.createBinder();
			
			return binder.unmarshal(node, OType.class);
		} catch (Exception e) {
			// TODO: handle exception
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Loading xml log files to java objects
	 * 
	 * @param inputTraceFolder
	 * @param maxNumberOfFiles
	 * @return
	 */
	public static List<Body> loadXMLLog(String inputTraceFolder,
			int maxNumberOfFiles) {
		File traceFolder = new File(inputTraceFolder);
		if (traceFolder.exists() && traceFolder.isDirectory()) {
			String[] fileList = traceFolder.list(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String fileName) {
					if (fileName.endsWith(".xml"))
						return true;
					return false;
				}
			});

			List<Body> retList = new ArrayList<Body>();
			int count = 0;
			try {
				if (jcontext == null)
					jcontext = JAXBContext.newInstance("eu.fittest.itelog");
			} catch (JAXBException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			for (String f : fileList) {

				try {
					Unmarshaller um = jcontext.createUnmarshaller();
					String logFile = inputTraceFolder + File.separator + f;
					Body body = (Body) um
							.unmarshal(new FileInputStream(logFile));

					count++;
					if (count > maxNumberOfFiles)
						break;
					else
						retList.add(body);

				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (JAXBException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			return retList;
		}

		return null;
	}
	
	/**
	 * Load a list of log files, with absolute path, stop if the limit reaches 
	 * 
	 * @param fileList
	 * @param maxNumberOfFiles
	 * @return
	 */
	public static List<Body> loadXMLLog(List<String> fileList,
			int maxNumberOfFiles) {
		
			List<Body> retList = new ArrayList<Body>();
			int count = 0;
			try {
				if (jcontext == null)
					jcontext = JAXBContext.newInstance("eu.fittest.itelog");
			} catch (JAXBException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			for (String file : fileList) {

				try {
					Unmarshaller um = jcontext.createUnmarshaller();
					Body body = (Body) um
							.unmarshal(new FileInputStream(file));

					count++;
					if (count > maxNumberOfFiles)
						break;
					else
						retList.add(body);

				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (JAXBException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			return retList;
		
	}
}
