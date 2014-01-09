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

import static org.junit.Assert.*;

import java.io.File;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

import eu.fittest.itelog.Body;
import eu.fittest.itelog.OType;
import eu.fittest.itelog.OType.Fd;

public class XMLUtilsTest {
	
	private String projectRoot = System.getProperty("user.dir", "."); 
	private String testDataFolder = projectRoot + File.separator
			+ "src" + File.separator
			+ "test" + File.separator
			+ "resources" + File.separator;
			
	
	@Ignore
	public void testQuery() {
		String logFolder = testDataFolder  + "data";
		
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 10);
		
		for (Body body : logBodies){
			List<OType> queryResults = XMLUtils.queryByType("id", "login_username", body);
			for (OType entry : queryResults){
				assertTrue(entry.getFd().size() > 0);
				
//				Fd dataEntry = entry.getFd().get(entry.getFd().size() - 1);
//				System.out.println(dataEntry.getV().getTy());
//				System.out.println(dataEntry.getV().getV());
			}
		}
		
	}

	@Ignore
	public void testQuery1() {
		String logFolder = testDataFolder + "ite";
		
		List<Body> logBodies = XMLUtils.loadXMLLog(logFolder, 10);
		
		for (Body body : logBodies){
			List<OType> queryResults = XMLUtils.queryByType("I", "2", body);
			for (OType entry : queryResults){
				assertTrue(entry.getFd().size() > 0);
				
//				Fd dataEntry = entry.getFd().get(entry.getFd().size() - 1);
//				System.out.println(dataEntry.getV().getTy());
//				System.out.println(dataEntry.getV().getV());
			}
		}
		
	}

}
