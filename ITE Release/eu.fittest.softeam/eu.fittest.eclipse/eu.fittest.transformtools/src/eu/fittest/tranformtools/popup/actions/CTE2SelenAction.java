package eu.fittest.tranformtools.popup.actions;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
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
import org.osgi.framework.Bundle;

import eu.fbk.se.selenium.renders.HttpElementRender;
import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2FlashSelenium;
import eu.fbk.se.transform.CTE2FlexSelenium;
import eu.fbk.se.transform.CTE2Http;
import eu.fbk.se.transform.CTE2Selenium;
import eu.fbk.se.transform.IProgressListener;
import eu.fbk.se.transform.ITranformer;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;
import eu.fbk.se.webelement.HttpElementBean;
import eu.fittest.project.config.TestProject;
import eu.fittest.tranformtools.Activator;
import eu.fittest.tranformtools.wizards.Constants;
import eu.fittest.tranformtools.wizards.SeleniumGenWizard;

public class CTE2SelenAction implements IObjectActionDelegate {

	private Shell shell;
	private IFile[] selectedFile;
	private boolean debug = false;

	/**
	 * Constructor for Action1.
	 */
	public CTE2SelenAction() {
		super();
	}

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 */
	public void run(IAction action) {

		if (debug) {
			System.out.println("run method called: ver 1.0");
		}

		if (selectedFile != null && selectedFile.length > 0) {
			final IFile file = selectedFile[0]; // first file only

			SeleniumGenWizard wizard = new SeleniumGenWizard(file,
					file.getProject());
			WizardDialog dialog = new WizardDialog(shell, wizard);
			if (dialog.open() == WizardDialog.OK) {

				final String packageName = wizard.getParameterPage()
						.getPackageName();
				final String className = wizard.getParameterPage()
						.getClassName();
				final String targetPage = wizard.getParameterPage()
						.getTargetPage();
				final String cteFile = wizard.getParameterPage().getCteFile();
				final String outputFolder = wizard.getParameterPage()
						.getOutputFolder();
				final String domainInputFile = wizard.getParameterPage()
						.getDomainInputFile();
				final String selenDriver = wizard.getParameterPage()
						.getSeleniumDriver();
				final boolean validTestOnly = wizard.getParameterPage()
						.isOnlyValidTestCase();

				if (debug) {
					System.out.println("selenDriver selected:"+selenDriver);
				}
				
				TestProject projectConfig = eu.fittest.eclipse.gui.Activator
						.getDefault().getActiveProjectConfig();
				
				if (debug) {
				
					System.out.println("projectConfig baseUrl:" + projectConfig.getGeneral().getBaseURL());
				
				}
				
				final String baseUrl = projectConfig != null ? projectConfig
						.getGeneral().getBaseURL()
						: "http://localhost:80";

				Job generateJob = new Job("Generate Test Case") {

					protected IStatus run(final IProgressMonitor monitor) {
						String templateGroupFile = null;
						Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);
						try {
							if (selenDriver
									.equals(Constants.FLEX_OBJECT_DRIVER)) {
								templateGroupFile = FileLocator
										.toFileURL(
												bundle.getEntry("templates/junit.flexdriver.stg"))
										.getFile();
							} else if (selenDriver
									.equals(Constants.FLASH_OBJECT_DRIVER)) {
								templateGroupFile = FileLocator
										.toFileURL(
												bundle.getEntry("templates/junit.flashdriver.stg"))
										.getFile();
							} else if (selenDriver
									.equals(Constants.HTML_UNIT_DRIVER)) {
								templateGroupFile = FileLocator
										.toFileURL(
												bundle.getEntry("templates/junit.httpunit.stg"))
										.getFile();
							} else {
								templateGroupFile = FileLocator
										.toFileURL(
												bundle.getEntry("templates/junit4.wdriver.stg"))
										.getFile();
							}
							AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(
									templateGroupFile);
							if (templateProvider.isTemplateReady()) {

								try {

									ITranformer transformer;

									if (selenDriver
											.equals(Constants.FLEX_OBJECT_DRIVER)) {

										transformer = new CTE2FlexSelenium(
												templateProvider, packageName,
												className, targetPage,
												selenDriver);

									} else if (selenDriver
											.equals(Constants.FLASH_OBJECT_DRIVER)) {

										transformer = new CTE2FlashSelenium(
												templateProvider, packageName,
												className, targetPage,
												selenDriver);

									} else if (selenDriver
											.equals(Constants.HTML_UNIT_DRIVER)) {

										templateProvider.registerRenderer(
												HttpElementBean.class,
												new HttpElementRender(baseUrl));

										transformer = new CTE2Http(
												templateProvider, packageName,
												className, targetPage,
												selenDriver);

									} else {
										transformer = new CTE2Selenium(
												templateProvider, packageName,
												className, targetPage,
												selenDriver);
									}

									transformer
											.register(new IProgressListener() {

												public void start(int totalWork) {
													monitor.beginTask(
															"Generating test cases",
															totalWork);

												}

												public void progress(int work) {
													monitor.worked(work);

												}

												public void finish() {
													monitor.done();

												}
											});

									if (domainInputFile == null
											|| domainInputFile.isEmpty()) {

										transformer.transform(cteFile,
												outputFolder, validTestOnly);

									} else {

										transformer.transform(cteFile,
												domainInputFile, outputFolder,
												validTestOnly);

									}

									shell.getDisplay().asyncExec(
											new Runnable() {

												public void run() {
													MessageDialog
															.openInformation(
																	shell,
																	"Test case generation",
																	"Test generation completed OK !");
												}
											});

								} catch (TransformException e) {
									shell.getDisplay().asyncExec(
											new Runnable() {

												public void run() {
													MessageDialog
															.openError(
																	shell,
																	"Test case generation",
																	"Error occur during the transformation, please check the input parameters!");
												}
											});
								}
							}
							try {
								file.getProject().refreshLocal(
										IResource.DEPTH_INFINITE, null);
							} catch (CoreException e) {
								e.printStackTrace();
							}
							return new Status(IStatus.OK,
									"FITTEST tranformation plugin", IStatus.OK,
									"Finish Generating Test Suites!", null);
						} catch (IOException e) {
							return new Status(IStatus.ERROR,
									"FITTEST tranformation plugin",
									IStatus.ERROR, e.getMessage(), null);
						}

					}
				};
				generateJob.schedule();
			}
		}
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
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
					MessageDialog
							.openInformation(
									shell,
									"Synchronization error",
									"The selected file is not synchronized with the system file, please refresh the project!");
				}
			}
		}

		if (newFiles.isEmpty()) {
			selectedFile = null;
			// action.setEnabled(false);
		} else {
			selectedFile = (IFile[]) newFiles
					.toArray(new IFile[newFiles.size()]);
			// action.setEnabled(true);
		}
	}

}
