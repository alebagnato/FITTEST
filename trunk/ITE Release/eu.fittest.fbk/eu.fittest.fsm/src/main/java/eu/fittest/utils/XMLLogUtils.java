package eu.fittest.utils;

import java.io.Reader;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.xml.sax.InputSource;

import eu.fittest.itelog.Body;

public class XMLLogUtils {
	
	private static JAXBContext jcontext = null;
	
	/**
	 * Load xml file from a reader
	 * @param reader
	 * @return
	 */
	public static Body load(Reader reader){
		try {
			if (jcontext == null)
				jcontext = JAXBContext.newInstance("eu.fittest.itelog");

			Unmarshaller um = jcontext.createUnmarshaller();
			Body body = (Body) um.unmarshal(new InputSource(reader));
			return body;
		} catch (JAXBException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
			
		return null;
	}
}
