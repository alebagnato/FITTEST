/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.fbk.efsm2ct.efsm2mon;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * 
 * @author tiella
 */
public class Version {

	private static String id = null;

	public static String getVersion() {

		if (id == null) {

			try {
				InputStream idStream = Version.class.getClassLoader().getResourceAsStream("id.txt");

				if (idStream != null) {

					BufferedReader br = new BufferedReader(new InputStreamReader(idStream));

					id = br.readLine();
					br.close();

				} else {
					id = "unknown [id.txt is missed]";
				}

			} catch (IOException ex) {

				id = "unknown: check id.txt";
			}

		}

		return id;
	}
}
