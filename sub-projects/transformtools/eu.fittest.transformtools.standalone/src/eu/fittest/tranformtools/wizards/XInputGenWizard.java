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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fbk.se.fsm.transformer.FSM2CTE;
import eu.fbk.se.fsm.transformer.IProgressListener;
import eu.fbk.se.fsm.visitor.VisitorFactory;
import eu.fbk.xinputmining.XinputMiner;
import eu.fittest.tranformtools.Activator;


public class XInputGenWizard extends Wizard {

	XinputPage xinputPage = null;
	String fsmFile = "";
	
	public XInputGenWizard(String fsmFile) {
		super();
		this.fsmFile = fsmFile;
	}

	
	public boolean performFinish() {
		if (xinputPage != null){
			
			
			final String fsmModel = xinputPage.getFsmModel();
			final String logFolder = xinputPage.getOutputFolder();
			final String domainInputs = xinputPage.getDomainInputFile();
			
			Job generateJob = new Job("Mining domain input specification"){

				protected IStatus run(final IProgressMonitor monitor) {					
					XinputMiner miner = new XinputMiner();
					
					miner.mine(fsmModel, logFolder, domainInputs);
					
					return new Status(IStatus.OK, "FITTEST tranformation plugin", 
							IStatus.OK, "Finish mining domain input file!", null);
				}
			};
			generateJob.schedule();

			return true;
		}
		return false;
		
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		xinputPage = new XinputPage("xinputPage");
		xinputPage.setTitle("Mining domain input spec. from FSM model and logs");
		xinputPage.setDescription("Select FSM model and output domain specification file");
		xinputPage.setImageDescriptor(imgDes);
		xinputPage.setFsmModel(fsmFile);
		
		addPage(xinputPage);
	}
	
	

}
