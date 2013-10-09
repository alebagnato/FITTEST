/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.transform;

import org.stringtemplate.v4.ST;

public class SeleniumDriverTemplateProvider extends AbstractTemplateProvider {

	public SeleniumDriverTemplateProvider(String templateFile) {
		super(templateFile);
	}

	@Override
	public ST getHeaderTemplate() {
		if (templateReady){
			return stgGroup.getInstanceOf("TestHeader");
		}
		return null;
	}

	@Override
	public ST getFooterTemplate() {
		if (templateReady){
			return stgGroup.getInstanceOf("TestFooter");
		}
		return null;
	}

	@Override
	public ST getTCTemplate() {
		if (templateReady){
			return stgGroup.getInstanceOf("TestCase");
		}
		return null;
	}

}
