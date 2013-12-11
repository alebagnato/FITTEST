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

import org.stringtemplate.v4.AttributeRenderer;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

public abstract class AbstractTemplateProvider {
	protected boolean templateReady;
	protected STGroup stgGroup;
	
	public AbstractTemplateProvider(String templateFile) {
		init(templateFile);
	}
	
	public abstract ST getHeaderTemplate();
	public abstract ST getFooterTemplate();
	public abstract ST getTCTemplate();

	/**
	 * Load template group files
	 * @param templateFile
	 */
	protected void init(String templateFile){
		try {
			stgGroup = new STGroupFile(templateFile);
			templateReady = true;
		} catch (Exception e){
			e.printStackTrace();
			templateReady = false;
		}
	}
	
	/**
	 * Register render
	 * @param clz
	 * @param render
	 */
	public void registerRenderer(Class clz, AttributeRenderer render){
		if (templateReady){
			stgGroup.registerRenderer(clz, render);
		}
	}

	public boolean isTemplateReady() {
		return templateReady;
	}
	
	
}
