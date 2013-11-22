package fbk.se.services.mutation.utils;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.xml.serialize.XMLSerializer;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

@SuppressWarnings("deprecation")
public class FileUtils {
	
	/**
	 * Save a DOM Document to a file
	 * @param doc
	 * @param filePath
	 */
	public static void saveDOMDocToFile(Document doc, String filePath){
		/*
		Source source = new DOMSource(doc);
		File out = new File(filePath);
//		Result result = new StreamResult(out);
		Result target = new StreamResult(out.toURI().getPath());
		
		// write doc to file
		try {
			TransformerFactory factory = TransformerFactory.newInstance();
			//factory.setAttribute(, 4);
			Transformer xformer = factory.newTransformer();
			
//			xformer.setOutputProperty(OutputKeys.METHOD, "xml");
//			xformer.setOutputProperty(OutputKeys.INDENT, "yes");
			xformer.transform(source, target);
		} catch (TransformerConfigurationException e) {
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			e.printStackTrace();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
		*/
		
		// use specific Xerces class to write DOM-data to a file:
		try {
		    XMLSerializer serializer = new XMLSerializer();
		    serializer.setOutputCharStream(new java.io.FileWriter(filePath));
			serializer.serialize(doc);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	/**
	 * Load an xml file to DOM Document
	 * @param filePath
	 * @return
	 */
	public static Document loadXMLfileToDOMDoc(String filePath){
		DocumentBuilder builder;
		try {
			builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			Document document = builder.parse(new File(filePath));
			return document;
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	/**
	 * Check if file or directory exist
	 * @param fileordir
	 * @return
	 */
	public static int checkFileDir(String fileordir){
		File f = new File(fileordir);
		if (!f.exists()) 
			return -1;
		else if (f.isDirectory())
			return 0;
		else return 1;
		
	}
}
