/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.tranformtools.wizards;

public class Constants {
	public static final String HTML_UNIT_DRIVER = "HtmlUnitDriver";
	public static final String HTML_FIREFOX_DRIVER = "FirefoxDriver";
	public static final String HTML_CHROME_DRIVER = "ChromeDriver";
	public static final String HTML_IE_DRIVER = "InternetExplorerDriver";
	public static final String FLEX_OBJECT_DRIVER = "FlexObjectDriver";
	public static final String FLASH_OBJECT_DRIVER = "FlashApplication";
	
//	public static final String[] driverList = {HTML_UNIT_DRIVER, HTML_FIREFOX_DRIVER,
//		HTML_CHROME_DRIVER, HTML_IE_DRIVER, FLEX_OBJECT_DRIVER, FLASH_OBJECT_DRIVER}; 
	
	// 16/02/2012 Cu:
	// Remove flex driver because it creates confusion with flash and it's not used for now
	// 
	public static final String[] driverList = {HTML_UNIT_DRIVER, HTML_FIREFOX_DRIVER,
		HTML_CHROME_DRIVER, HTML_IE_DRIVER, FLASH_OBJECT_DRIVER}; 
}
