/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.tranformtools.popup.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2FlashSelenium;
import eu.fbk.se.transform.CTE2FlexSelenium;
import eu.fbk.se.transform.CTE2RobotiumAndroid;
import eu.fbk.se.transform.CTE2Selenium;
import eu.fbk.se.transform.IProgressListener;
import eu.fbk.se.transform.ITranformer;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;
import eu.fittest.tranformtools.Activator;
import eu.fittest.tranformtools.wizards.AndroidGenWizard;
import eu.fittest.tranformtools.wizards.Constants;
import eu.fittest.tranformtools.wizards.SeleniumGenWizard;

public class CTE2AndroidAction implements IObjectActionDelegate {
	
	private Shell shell;
	private IFile[] selectedFile; 
	
	@Override
	public void run(IAction action) {
		if (selectedFile != null && selectedFile.length > 0) {
			IFile file = selectedFile[0]; // first file only
			
			AndroidGenWizard wizard = new AndroidGenWizard(file.getLocation().toOSString());
			WizardDialog dialog = new WizardDialog(shell, wizard);
			if (dialog.open() == WizardDialog.OK){
				
				final String packageName = wizard.getParamPage().getPackageName();
				final String className = wizard.getParamPage().getClassName();
				
				final String cteFile = wizard.getParamPage().getCteFile();
				final String outputFolder = wizard.getParamPage().getOutputFolder();
				final String domainInputFile = wizard.getParamPage().getDomainInputFile();

				final boolean validTestOnly = wizard.getParamPage().isOnlyValidTestCase();
				
				final String androidPackage = wizard.getParamPage().getAndroidTargetPackage();
				final String androidActivity = wizard.getParamPage().getAndroidTargetActivity();
				
				
				Job generateJob = new Job("Generate Test Case"){

					
					protected IStatus run(final IProgressMonitor monitor) {
						String templateGroupFile = Activator.getPluginPath()
								.toOSString()
								+ File.separator
								+ "templates"
								+ File.separator
								+ "robotium.driver.stg";
						AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(templateGroupFile);
						if (templateProvider.isTemplateReady()){
							
							try {
								
								ITranformer transformer = new CTE2RobotiumAndroid(
										templateProvider,
										packageName,
										className,
										androidPackage,
										androidActivity);
								
								transformer.register(new IProgressListener() {
									
									public void start(int totalWork) {
										monitor.beginTask("Generating test cases", totalWork);
										
									}
									public void progress(int work) {
										monitor.worked(work);
										
									}
									public void finish() {
										monitor.done();
										
									}
								});
		
								if (domainInputFile == null || domainInputFile.isEmpty()){
									transformer.transform(cteFile, outputFolder, validTestOnly);
								} else {
									transformer.transform(cteFile, domainInputFile, outputFolder, validTestOnly);
								}

								shell.getDisplay().asyncExec(new Runnable() {
									
									
									public void run() {
										MessageDialog.openInformation(shell, "Test case generation", 
										"Test generation completed successfully !");
									}
								});
								
							} catch (TransformException e) {
								shell.getDisplay().asyncExec(new Runnable() {

									
									public void run() {
										MessageDialog.openError(shell,
														"Test case generation",
														"Error occur during the transformation, please check the input parameters!");
									}
								});
							}	
						}
						
						return new Status(IStatus.OK, "FITTEST tranformation plugin", 
								IStatus.OK, "Finish Generating Test Suites!", null);
					}
				};
				generateJob.schedule();
			}
		}
		
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		ArrayList<IFile> newFiles = new ArrayList<IFile>();
		IFile file = null;
		if (selection != null && selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;

			for (Iterator iter = ss.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof IFile) {
					file = (IFile) obj;
				} else if (obj instanceof IAdaptable) {
					IAdaptable a = (IAdaptable) obj;
					IResource res = (IResource) a.getAdapter(IResource.class);
					if (res instanceof IFile) {
						file = (IFile) res;
					}
				}

				if (file != null && file.isSynchronized(IResource.DEPTH_ZERO)) {
					newFiles.add(file);
				} else if (!file.isSynchronized(IResource.DEPTH_ZERO)) {
					MessageDialog.openInformation(shell, "Synchronization error",
					"The selected file is not synchronized with the system file, please refresh the project!");
				}
			}
		}

		if (newFiles.isEmpty()) {
			selectedFile = null;
			// action.setEnabled(false);
		} else {
			selectedFile = (IFile[]) newFiles.toArray(new IFile[newFiles.size()]);
			// action.setEnabled(true);
		}
		
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
		
	}

}
