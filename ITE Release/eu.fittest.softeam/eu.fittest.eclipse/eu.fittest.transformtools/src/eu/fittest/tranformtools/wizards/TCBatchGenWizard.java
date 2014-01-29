package eu.fittest.tranformtools.wizards;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.osgi.framework.Bundle;

import eu.fbk.se.selenium.renders.HttpElementRender;
import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2FlashSelenium;
import eu.fbk.se.transform.CTE2FlexSelenium;
import eu.fbk.se.transform.CTE2Http;
import eu.fbk.se.transform.CTE2Selenium;
import eu.fbk.se.transform.ITranformer;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;
import eu.fbk.se.webelement.HttpElementBean;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.project.config.TestProject;
import eu.fittest.tranformtools.Activator;

public class TCBatchGenWizard extends Wizard {

	private Shell shell;
	BatchGenPage paramPage;
	String cteFolder;
	IProject currentProject;
	private boolean debug = false;

	public TCBatchGenWizard(Shell shell, String cteFolder,
			IProject currentProject) {
		super();
		this.shell = shell;
		this.cteFolder = cteFolder;
		this.currentProject = currentProject;
	}

	@Override
	public boolean performFinish() {

		if (paramPage != null) {
			cteFolder = paramPage.getCteFolder();
			File f = new File(cteFolder);

			final String[] fileList = f.list(new FilenameFilter() {
				public boolean accept(File dir, String name) {
					return name.endsWith(".cte");
				}
			});

			if (fileList != null && fileList.length > 0) {

				TestProject projectConfig = eu.fittest.eclipse.gui.Activator
						.getDefault().getActiveProjectConfig();
				
				if (debug ) {
				
					System.out.println("projectConfig baseUrl:" + projectConfig.getGeneral().getBaseURL());
				
				}
				
				final String baseUrl = projectConfig != null ? projectConfig
						.getGeneral().getBaseURL()
						: "http://localhost:80";
				
				Job generateJob = new Job("Generate Test Cases") {

					@Override
					protected IStatus run(final IProgressMonitor monitor) {

						monitor.beginTask("Generating test cases...",
								fileList.length);
						String selenDriver = paramPage.getSeleniumDriver();
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

							for (String s : fileList) {
								String className = s.replace(".cte", "")
										.toUpperCase() + "Test";
								AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(
										templateGroupFile);

								if (templateProvider.isTemplateReady()) {
									ITranformer transformer;
									try {
										if (selenDriver
												.equals(Constants.FLEX_OBJECT_DRIVER)) {
											transformer = new CTE2FlexSelenium(
													templateProvider,
													paramPage.getPackageName(),
													className,
													paramPage.getTargetPage(),
													selenDriver);
										} else if (selenDriver
												.equals(Constants.FLASH_OBJECT_DRIVER)) {
											transformer = new CTE2FlashSelenium(
													templateProvider,
													paramPage.getPackageName(),
													className,
													paramPage.getTargetPage(),
													selenDriver);
										} else if (selenDriver
												.equals(Constants.HTML_UNIT_DRIVER)) {

											templateProvider.registerRenderer(
													HttpElementBean.class,
													new HttpElementRender(baseUrl));

											transformer = new CTE2Http(
													templateProvider, paramPage.getPackageName(),
													className, paramPage.getTargetPage(),
													selenDriver);

										} else {
											transformer = new CTE2Selenium(
													templateProvider,
													paramPage.getPackageName(),
													className,
													paramPage.getTargetPage(),
													selenDriver);
										}
										String cteFile = cteFolder
												+ File.separator + s;
										if (paramPage.getDomainInputFile() == null
												|| paramPage
														.getDomainInputFile()
														.isEmpty()) {
											transformer
													.transform(
															cteFile,
															paramPage
																	.getOutputFolder(),
															paramPage
																	.isOnlyValidTestCase());
										} else {
											transformer
													.transform(
															cteFile,
															paramPage
																	.getDomainInputFile(),
															paramPage
																	.getOutputFolder(),
															paramPage
																	.isOnlyValidTestCase());
										}

										monitor.worked(1);

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
										monitor.done();
									}

								}
							}
							monitor.done();
							try {
								currentProject.refreshLocal(
										IResource.DEPTH_INFINITE, null);
							} catch (CoreException e) {
								e.printStackTrace();
							}
							shell.getDisplay().asyncExec(new Runnable() {
								public void run() {
									MessageDialog
											.openInformation(shell,
													"Test case generation",
													"Test generation completed sucessfully !");
								}
							});

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

		return true;
	}

	@Override
	public void addPages() {
		ImageDescriptor imgDes = Activator.getImageDescriptor("icons"
				+ File.separator + "fittest-logo-small.jpg");
		paramPage = new BatchGenPage("batchGenPage");
		paramPage.setTitle("Generate test cases from CTE trees");
		paramPage.setDescription("Specifying parameters");
		paramPage.setImageDescriptor(imgDes);
		paramPage.setPackageName(new File(cteFolder).getName());
		paramPage.setCteFolder(cteFolder);
		paramPage.setOutputFolder(currentProject
				.getFile(IFITTESTFolderConstants.TEST_SUITES).getLocation()
				.toOSString());

		addPage(paramPage);

	}

}
