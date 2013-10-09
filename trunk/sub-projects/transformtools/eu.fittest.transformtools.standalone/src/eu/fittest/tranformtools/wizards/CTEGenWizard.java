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
import eu.fittest.tranformtools.Activator;


public class CTEGenWizard extends Wizard {

	PathPage pathPage = null;
	String fsmFile = "";
	
	public CTEGenWizard(String fsmFile) {
		super();
		this.fsmFile = fsmFile;
	}

	
	public boolean performFinish() {
		if (pathPage != null){
			
			
			final String fsmModel = pathPage.getFsmModel();
			final String outFolder = pathPage.getOutputFolder();
			final String domainInputs = pathPage.getDomainInputFile();
			final boolean shouldGenerateTestCase = pathPage.isShouldGenerateTestCase();
			final boolean shouldOptimizeTestCase = pathPage.isShouldOptimizeTestCase();
			final String visitorName = pathPage.getVisitorName();
			final String visitorId = VisitorFactory.VISITORS.get(visitorName);
			final String traceFolder = pathPage.getTraceFolder();
			
			Job generateJob = new Job("Generating CTE trees"){

				protected IStatus run(final IProgressMonitor monitor) {					
					FSM2CTE transformer = new FSM2CTE(visitorId);
					
					transformer.register(new IProgressListener() {
						
						
						public void start(int totalWork) {
							monitor.beginTask("Generating CTE trees", totalWork);
							
						}
						
						
						public void progress(int work) {
							monitor.worked(work);
							
						}
						
						
						public void finish() {
							monitor.done();
							
						}
					});
					
					if (domainInputs == null || domainInputs.isEmpty()){
						transformer.transform(fsmModel, outFolder, traceFolder);
					} else {
						transformer.transform(domainInputs, fsmModel, outFolder, shouldGenerateTestCase, shouldOptimizeTestCase, traceFolder);
					}
					
					return new Status(IStatus.OK, "FITTEST tranformation plugin", 
							IStatus.OK, "Finish Generating CTE trees!", null);
				}
			};
			generateJob.schedule();

			return true;
		}
		return false;
		
	}

	
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		pathPage = new PathPage("xinputPage");
		pathPage.setTitle("Generate CTE trees from FSM model");
		pathPage.setDescription("Select FSM model and output folder");
		pathPage.setImageDescriptor(imgDes);
		pathPage.setFsmModel(fsmFile);
		
		addPage(pathPage);
	}
	
	

}
