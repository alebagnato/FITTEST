/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.converter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FilenameFilter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.w3c.xhtml1.Html;
import org.w3c.xhtml1.Table;
import org.w3c.xhtml1.Tbody;
import org.w3c.xhtml1.Tr;

import eu.fittest.itelog.Body;
import eu.fittest.itelog.EType;
import eu.fittest.itelog.OType;
import eu.fittest.itelog.OType.Fd;
import eu.fittest.itelog.VType;

public class SeleniumHtml2ITELogConverter {

	/**
	 * Convert the whole directory of selenium html test cases to ITE xml logs.
	 * 
	 * @param inputDir
	 * @param outputDir
	 * @throws LogConverterException
	 */
	public void convertAll(String inputDir, String outputDir)
			throws LogConverterException {

		File dir = new File(inputDir);
		if (dir.exists() && dir.isDirectory()) {
			File[] listInputFiles = dir.listFiles(new FilenameFilter() {

				@Override
				public boolean accept(File dir, String fileName) {
					if (fileName.endsWith(".html"))
						return true;

					return false;
				}
			});

			String absOurDir = outputDir + File.separator + "abstract";
			String concreteOurDir = outputDir + File.separator + "concrete";

			File outDir = new File(concreteOurDir);
			if (!outDir.exists()) {
				outDir.mkdirs();
			}

			outDir = new File(absOurDir);
			if (!outDir.exists()) {
				outDir.mkdirs();
			}

			for (File inputFile : listInputFiles) {
				String outputFileName = inputFile.getName().replace(".html",
						".xml");
				String concreteFileName = concreteOurDir + File.separatorChar
						+ outputFileName;
				String absFileName = absOurDir + File.separatorChar
						+ outputFileName;
				convert(inputFile.getAbsolutePath(), concreteFileName,
						absFileName);
			}

		} else {
			throw new LogConverterException(
					"Input directory does not exist or is not accessible!");
		}

	}

	/**
	 * Convert a single selenium html test case to a
	 * 
	 * @param seleniumHtmlFile
	 * @param concreteOutputFile
	 * @return
	 */
	public boolean convert(String seleniumHtmlFile, String concreteOutputFile,
			String absOutputFile) {

		Html inputObject = readHTML(seleniumHtmlFile);

		// convert only the recognizable selenium html test cases
		if (inputObject != null) {
			if (inputObject.getBody() != null) {
				if (inputObject.getBody().getTable() != null
						&& inputObject.getBody().getTable().size() > 0) {
					Object tbl = inputObject.getBody().getTable().get(0);

					if (tbl instanceof Table) {
						Table testContent = (Table) tbl;
						if (testContent.getTbody() != null
								&& testContent.getTbody().size() > 0) {
							Tbody tblBody = testContent.getTbody().get(0);

							Body outLogBody = new Body();

							for (Tr row : tblBody.getTr()) {
								if (row.getThOrTd() != null
										&& row.getThOrTd().size() == 3) {
									String action = row.getThOrTd().get(0)
											.getValue();
									String ref = row.getThOrTd().get(1)
											.getValue();
									String value = row.getThOrTd().get(2)
											.getValue();

									EType elem = new EType();
									outLogBody.getE().add(elem);

									OType elemContent = new OType();
									elemContent.setTy("Struct");
									elem.getO().add(elemContent);

									Fd fdAction = new Fd();
									fdAction.setN("action");
									VType actionV = new VType();
									actionV.setTy("string");
									actionV.setV(action);
									fdAction.setV(actionV);
									elemContent.getFd().add(fdAction);

									Fd fdParams = new Fd();
									fdParams.setN("parameters");
									elemContent.getFd().add(fdParams);
									OType arrayO = new OType();
									arrayO.setTy("Array");
									fdParams.setO(arrayO);

									if (ref != null) {
										Fd id = new Fd();
										arrayO.getFd().add(id);
										VType idV = new VType();
										id.setV(idV);

										if (ref.startsWith("/")
												&& !ref.startsWith("//")) {
											id.setN("url");
											idV.setTy("url");
											idV.setV(ref);
										} else if (ref.startsWith("//")) {
											id.setN("xpath");
											idV.setTy("xpath");
											idV.setV(ref);
										} else if (ref.startsWith("id=")) {
											id.setN("id");
											idV.setTy("ID");
											idV.setV(ref.substring(3));
										} else {
											String[] idNvalue = ref.split("=",
													2);
											idV.setTy("string");
											// System.out.println(idNvalue.length);
											if (idNvalue.length == 2) {
												id.setN(idNvalue[0]);
												idV.setV(idNvalue[1]);
											} else {
												id.setN("text");
												idV.setV(ref);
											}
										}
									}

									if (value != null && value.length() > 0) {
										Fd text = new Fd();
										text.setN("text");
										arrayO.getFd().add(text);
										VType textV = new VType();
										text.setV(textV);
										if (isInt(value)){
											textV.setTy("int");
										} else if (isFloat(value)){
											textV.setTy("float");
										} else {
											textV.setTy("string");
										}
										textV.setV(value);
									}

								}
							}

							// Save Body to file
							saveITELog(outLogBody, concreteOutputFile);
							Body absBody = createAbsBody(inputObject);
							saveITELog(absBody, absOutputFile);

							return true;
						}

					}

				}

			}

		}

		return false;
	}

	private Body createAbsBody(Html inputObject) {
		Body outAbsBody = new Body();

		Object tbl = inputObject.getBody().getTable().get(0);

		if (tbl instanceof Table) {
			Table testContent = (Table) tbl;
			if (testContent.getTbody() != null
					&& testContent.getTbody().size() > 0) {
				Tbody tblBody = testContent.getTbody().get(0);

				for (Tr row : tblBody.getTr()) {
					if (row.getThOrTd() != null && row.getThOrTd().size() == 3) {
						String action = row.getThOrTd().get(0).getValue();
						String ref = row.getThOrTd().get(1).getValue();

						EType elem = new EType();
						outAbsBody.getE().add(elem);

						OType elemContent = new OType();
						elemContent.setTy("Struct");
						elem.getO().add(elemContent);

						Fd fdAction = new Fd();
						fdAction.setN("action");
						VType actionV = new VType();
						actionV.setTy("string");
						actionV.setV(action);
						fdAction.setV(actionV);
						elemContent.getFd().add(fdAction);

						Fd fdParams = new Fd();
						fdParams.setN("parameters");
						elemContent.getFd().add(fdParams);
						OType arrayO = new OType();
						arrayO.setTy("Array");
						fdParams.setO(arrayO);

						if (ref != null) {
							Fd id = new Fd();
							arrayO.getFd().add(id);
							VType idV = new VType();
							id.setV(idV);

							if (ref.startsWith("/") && !ref.startsWith("//")) {
								id.setN("url");
								idV.setTy("url");
								idV.setV(ref);
							} else if (ref.startsWith("//")) {
								id.setN("xpath");
								idV.setTy("xpath");
								idV.setV(ref);
							} else if (ref.startsWith("id=")) {
								id.setN("id");
								idV.setTy("ID");
								idV.setV(ref.substring(3));
							} else {
								String[] idNvalue = ref.split("=", 2);
								idV.setTy("string");
								// System.out.println(idNvalue.length);
								if (idNvalue.length == 2) {
									id.setN(idNvalue[0]);
									idV.setV(idNvalue[1]);
								} else {
									id.setN("text");
									idV.setV(ref);
								}
							}
						}
					}
				}
			}
		}

		return outAbsBody;
	}

	JAXBContext seleniumContext = null;
	Unmarshaller seleniumUnmarshaller = null;

	/**
	 * Load Selenium test case in html format to Html java object
	 * 
	 * @param fileName
	 * @return
	 */
	private Html readHTML(String fileName) {
		try {
			if (seleniumContext == null || seleniumUnmarshaller == null) {
				seleniumContext = JAXBContext.newInstance("org.w3c.xhtml1");
				seleniumUnmarshaller = seleniumContext.createUnmarshaller();
			}

			Html ret = (Html) seleniumUnmarshaller
					.unmarshal(new FileInputStream(fileName));
			return ret;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	JAXBContext iteContext = null;
	Marshaller iteMarshaller = null;

	/**
	 * Save ite log object to xml file
	 * 
	 * @param iteLogObject
	 * @param outputFileName
	 * @return
	 */
	private boolean saveITELog(Body iteLogObject, String outputFileName) {
		try {
			if (iteContext == null || iteMarshaller == null) {
				iteContext = JAXBContext.newInstance("eu.fittest.itelog");
				iteMarshaller = iteContext.createMarshaller();
				iteMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
						Boolean.TRUE);
			}
			// ObjectFactory factory = new ObjectFactory();

			iteMarshaller.marshal(iteLogObject, new FileWriter(outputFileName));
			return true;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	private boolean isInt(String value){
		try{
			int tmp = Integer.parseInt(value);
			return true;
		} catch (NumberFormatException e){
			return false;
		}
	}

	private boolean isFloat(String value){
		try{
			float tmp = Float.parseFloat(value);
			return true;
		} catch (NumberFormatException e){
			return false;
		}
	}

	/**
	 * Main method
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length == 2) {
			SeleniumHtml2ITELogConverter converter = new SeleniumHtml2ITELogConverter();
			try {
				converter.convertAll(args[0], args[1]);
			} catch (LogConverterException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			System.out
					.println("Usage: SeleniumHtml2ITELogConverter inputDir outputDir");
		}
	}

}
