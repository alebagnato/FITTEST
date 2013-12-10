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

import java.io.File;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.tranformtools.Activator;

public class FSMGenWizard extends Wizard {

	private FSMPage paramPage = null;
	
	public boolean performFinish() {
		if (paramPage == null) return false;
		return true;
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		paramPage = new FSMPage("Specify input / output");
		paramPage.setDescription("Specify log folder and output file to store inferred model");
		paramPage.setImageDescriptor(imgDes);
		
		addPage(paramPage);
	}

	public FSMPage getParamPage() {
		return paramPage;
	}
	
}
