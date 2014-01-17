/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
