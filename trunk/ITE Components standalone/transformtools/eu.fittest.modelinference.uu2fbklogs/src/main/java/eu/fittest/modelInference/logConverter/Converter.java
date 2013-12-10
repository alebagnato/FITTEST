/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.logConverter;

import java.io.BufferedWriter;

import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.*;

/**
 * Convert UU logs to FBK (model inference) logs
 * 
 * @author Alessandro Marchetto
 * 
 */
public class Converter {
	ABSstateDetection absChecker = new ABSstateDetection();
	Utility utils = new Utility();

	public void convert(File fXmlFile, String outputFileName) {
		try {

			// --- setting
			// String
			// filePath="input"+System.getProperty("file.separator")+"toplog_last.xml";
			// String
			// filePath="input"+System.getProperty("file.separator")+"toplog_last-CAMBIATO.xml";
			// //mia
			// String
			// filePath="input"+System.getProperty("file.separator")+"try2.xml";
			// String outputFileName="log_try2";

			// --------------

			utils.startFile(outputFileName);
			utils.writeLine("onLoad:__:load:__:[undef;__;]");
			// utils.writeLine(":__:onLoad:__:");

			// File fXmlFile = new File(filePath);

			DocumentBuilderFactory dbFactory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			Document doc = dBuilder.parse(fXmlFile);
			doc.getDocumentElement().normalize();

			NodeList bodyChilds = doc.getChildNodes().item(0).getChildNodes();

			String stateElement = "";
			String[] eventElement = new String[2];
			eventElement[0] = "";
			eventElement[1] = "";
			String logLine = "";
			// String lastDetectedEvent="onLoad";
			String lastDetectedState = "undef;__;";

			// int max=bodyChilds.getLength();

			for (int temp = 0; temp < bodyChilds.getLength(); temp++) {

				Node node_E = bodyChilds.item(temp);

				if (node_E.getNodeName().equalsIgnoreCase("E")) {
					if (node_E.getNodeType() == Node.ELEMENT_NODE) {
						Element childElement_E = (Element) node_E;
						// System.out.println(childElement_E.getNodeName());
						// //--> E

						NodeList childElement_O = childElement_E
								.getElementsByTagName("O");

						for (int i_o = 0; i_o < childElement_O.getLength(); i_o++) {

							Element o = (Element) childElement_O.item(i_o);
							// System.out.print(o.getAttribute("ty"));

							if (o.getAttribute("ty").equals("AppAbstractState")) {
								// processiamo stato
								stateElement = "";

								NodeList childElement_fd = o
										.getElementsByTagName("fd");

								stateElement = getStateValue(childElement_fd);

							} else if (o
									.getAttribute("ty")
									.equals("eu.fittest.actionscript.automation::RecordEvent")) {
								// processiamo evento

								eventElement = new String[2];
								eventElement[0] = "";
								eventElement[1] = "";
								NodeList childElement_fd = o
										.getElementsByTagName("fd");

								eventElement = getEventValue(childElement_fd);
							}

						}

					}

					// if
					// ((!eventIdElement.equalsIgnoreCase(""))&&(!stateElement.equalsIgnoreCase("")))
					// {
					if ((!eventElement[0].equalsIgnoreCase(""))
							&& (!eventElement[1].equalsIgnoreCase(""))
							&& (!stateElement.equalsIgnoreCase(""))) {
						/*
						 * logLine=":__:"+lastDetectedEvent+":__:["+stateElement+
						 * "]"; lastDetectedEvent=eventIdElement;
						 * utils.writeLine(logLine);
						 */
						logLine = eventElement[1] + ":__:" + eventElement[0]
								+ ":__:[" + lastDetectedState + "]";
						// lastDetectedEvent=eventElement[];
						lastDetectedState = stateElement;
						utils.writeLine(logLine);
					}
				}
			}

			// logLine=":__:"+lastDetectedEvent+":__:[;__;]";
			logLine = "onunLoad:__:" + "load" + ":__:[" + lastDetectedState
					+ "]";
			utils.writeLine(logLine);

			utils.closeFile();
			System.out.println("..convertion done!");
		} catch (Exception e) {
			System.out.println("..convertion not done!");
			e.printStackTrace();
		}
	}

	String[] getEventValue(NodeList childElement_fd) {
		String eventIdElement = "";
		String eventType = "";
		for (int i_fd = 0; i_fd < childElement_fd.getLength(); i_fd++) {
			Element fd = (Element) childElement_fd.item(i_fd);
			if (fd.getAttribute("n").equalsIgnoreCase("targetID")) {

				NodeList childElement_V = fd.getElementsByTagName("V");

				for (int i_V = 0; i_V < childElement_V.getLength(); i_V++) {

					Element V = (Element) childElement_V.item(i_V);
					if ((V.hasAttribute("v")) && (V.hasAttribute("ty"))) {
						eventIdElement = V.getAttribute("v");
						eventIdElement = eventIdElement.replace("\"", "");
						// return eventIdElement;
						break;
					}

				}

			} else if (fd.getAttribute("n").equalsIgnoreCase("type")) {

				NodeList childElement_V = fd.getElementsByTagName("V");

				for (int i_V = 0; i_V < childElement_V.getLength(); i_V++) {

					Element V = (Element) childElement_V.item(i_V);
					if ((V.hasAttribute("v")) && (V.hasAttribute("ty"))) {
						eventType = V.getAttribute("v");
						eventType = eventType.replace("\"", "");
						// return eventType;
						break;
					}

				}

			}

		}
		// return eventIdElement+"_"+eventType;
		String[] st = new String[2];
		st[0] = eventType;
		st[1] = eventIdElement;
		return st;
	}

	String getStateValue(NodeList childElement_fd) {
		String stateElements = "";
		String concreteValueOfVariable;
		String typeOfVariable;
		String abstractValueOfVariable;
		String fieldStateElements = "";
		String nameOfVariable = "";

		for (int i_fd = 0; i_fd < childElement_fd.getLength(); i_fd++) {

			Element fd = (Element) childElement_fd.item(i_fd);

			if (!fd.getAttribute("n").equalsIgnoreCase("I")) {

				nameOfVariable = fd.getAttribute("n");

				NodeList childElement_fdChild = fd.getChildNodes();

				for (int i_fdChild = 0; i_fdChild < childElement_fdChild
						.getLength(); i_fdChild++) {

					if (childElement_fdChild.item(i_fdChild).getNodeType() == Node.ELEMENT_NODE) {
						Element fdChild = (Element) childElement_fdChild
								.item(i_fdChild);

						fieldStateElements = "";

						if (fdChild.getNodeName().equalsIgnoreCase("O")) {
							// field Array con sottofield V

							NodeList childElement_O = fdChild
									.getElementsByTagName("O");

							for (int i_o = 0; i_o < childElement_O.getLength(); i_o++) {

								Element o = (Element) childElement_O.item(i_o);

								if (o.getAttribute("ty").equals("Array")) {
									NodeList childElement_fd2 = o
											.getElementsByTagName("fd");

									String arraystate = getStateValue(childElement_fd2);

									abstractValueOfVariable = absChecker
											.absCheckerAndSelection("array"
													.trim().toLowerCase(),
													arraystate);
									fieldStateElements = fieldStateElements
											+ "" + nameOfVariable + ":"
											+ "Array" + "="
											+ abstractValueOfVariable + ";__;";

									stateElements = stateElements
											+ fieldStateElements;
								}
							}

						} else if (fdChild.getNodeName().equalsIgnoreCase("V")) {
							{
								// field V

								if ((fdChild.hasAttribute("v"))
										&& (fdChild.hasAttribute("ty"))) {
									concreteValueOfVariable = fdChild
											.getAttribute("v");
									concreteValueOfVariable = concreteValueOfVariable
											.replace("\"", "");
									typeOfVariable = fdChild.getAttribute("ty");

									abstractValueOfVariable = absChecker
											.absCheckerAndSelection(
													typeOfVariable.trim()
															.toLowerCase(),
													concreteValueOfVariable
															.trim()
															.toLowerCase());
									fieldStateElements = fieldStateElements
											+ "" + nameOfVariable + ":"
											+ typeOfVariable + "="
											+ abstractValueOfVariable + ";__;";

								}

								stateElements = stateElements
										+ fieldStateElements;
							}

						}

					}

				}

			}

		}
		return stateElements;
	}

}
