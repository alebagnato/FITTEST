/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.fsm.utils;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import eu.fbk.se.fsm.cte.CteObject;
import eu.fbk.se.fsm.xinput.DomainInputs;

public class JAXBUtil {

	/**
	 * Save to cte file
	 * 
	 * @param object
	 * @param fileName
	 */
	public static void saveCte(CteObject object, String fileName) {
		JAXBContext jcontext;
		try {
			jcontext = JAXBContext.newInstance("eu.fbk.se.fsm.cte");
			Marshaller m = jcontext.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			// ObjectFactory factory = new ObjectFactory();

			m.marshal(object, new FileWriter(fileName));

		} catch (JAXBException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	
	/**
	 * Load cte file to object
	 * 
	 * @param fileName
	 * @return
	 */
	public static CteObject loadCte(String fileName) {

		try {
			JAXBContext jct = JAXBContext.newInstance("eu.fbk.se.fsm.cte");
			Unmarshaller um = jct.createUnmarshaller();

			CteObject ret = (CteObject) um.unmarshal(new FileInputStream(
					fileName));
			return ret;
			// return ret.getValue();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Load domain input specification
	 * 
	 * @param fileName
	 * @return
	 */
	public static DomainInputs loadDomainInputs(String fileName) {
		
		try {
			JAXBContext jct = JAXBContext.newInstance("eu.fbk.se.fsm.xinput");
			Unmarshaller um = jct.createUnmarshaller();
			
			DomainInputs ret = (DomainInputs) um.unmarshal(new FileInputStream(
					fileName));
			return ret;
			// return ret.getValue();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Save domain input spec to an XML file
	 * @param object
	 * @param fileName
	 */
	public static void saveDomainInputs(DomainInputs object, String fileName) {
		
		try {
			JAXBContext jct = JAXBContext.newInstance("eu.fbk.se.fsm.xinput");
			Marshaller m = jct.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			m.setProperty(Marshaller.JAXB_SCHEMA_LOCATION, "");
			m.marshal(object, new FileWriter(fileName));
			// return ret.getValue();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String projectPath = System.getProperty("user.dir");
		CteObject cteObject = loadCte(projectPath + "/xsd/P1_1.cte");
		System.out.println("Tree id = " + cteObject.getTree());
		saveCte(cteObject, projectPath + "/xsd/P1_1_copy.cte");
	}

}
