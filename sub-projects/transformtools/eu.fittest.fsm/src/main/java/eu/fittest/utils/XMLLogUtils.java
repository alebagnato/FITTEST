/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
