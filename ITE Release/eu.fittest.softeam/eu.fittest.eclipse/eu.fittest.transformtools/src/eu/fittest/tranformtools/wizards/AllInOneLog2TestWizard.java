package eu.fittest.tranformtools.wizards;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.eclipse.core.internal.resources.Folder;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.osgi.framework.Bundle;

import eu.fbk.se.fsm.transformer.FSM2CTE;
import eu.fbk.se.fsm.visitor.VisitorFactory;
import eu.fbk.se.transform.AbstractTemplateProvider;
import eu.fbk.se.transform.CTE2FlashSelenium;
import eu.fbk.se.transform.CTE2FlexSelenium;
import eu.fbk.se.transform.CTE2Selenium;
import eu.fbk.se.transform.ITranformer;
import eu.fbk.se.transform.SeleniumDriverTemplateProvider;
import eu.fbk.se.transform.TransformException;
import eu.fbk.xinputmining.XinputMiner;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eventBasedFSM.EventBasedFSM;
import eu.fittest.modelInference.fsmInference.config.Config_Launcher;
import eu.fittest.modelInference.fsmInference.manager.FSMmanager;
import eu.fittest.project.config.BrowserType;
import eu.fittest.project.config.InferenceTechniqueType;
import eu.fittest.project.config.ModelVisitStrategyType;
import eu.fittest.project.config.TestProject;
import eu.fittest.tranformtools.utils.LogFileUtils;

public class AllInOneLog2TestWizard extends Wizard implements INewWizard {

	private IProject selectedProject;
	private TestProject projectConfig;
	private String selectedProjectRootDir;
	private IPath tmpFolder; 
	
	private AllInOneLog2TestPage selectionPage;
	
	private Shell shell;
	
	public AllInOneLog2TestWizard() {
		setWindowTitle("Log to test cases: all in one inference/generation wizard");
		projectConfig = Activator.getDefault().getActiveProjectConfig();
	}	

	public void init(IWorkbench workbench, IProject selectedProject){
		this.selectedProject = selectedProject;
		selectedProjectRootDir = selectedProject.getLocation().toOSString() + File.separator;
		shell = workbench.getActiveWorkbenchWindow().getShell();
	}
	
	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		if(selection.getFirstElement()!=null){
			this.selectedProject = ((IResource)selection.getFirstElement()).getProject();
			selectedProjectRootDir = selectedProject.getLocation().toOSString() + File.separator;
		}
	}
	

	@Override
	public boolean performFinish() {
		if (projectConfig != null){
			
			final boolean performModelInfer = selectionPage.isModelInferenceSelected(); 
			final boolean performXinputMining = selectionPage.isXinputMiningSelected(); 
			final boolean performCTEGen = selectionPage.isCTEGenSelected(); 
			final boolean performTestGen = selectionPage.isSelenGenSelected(); 
			
			try {
			IRunnableWithProgress op = new IRunnableWithProgress() {
				
				@Override
				public void run(IProgressMonitor monitor) throws InvocationTargetException,
						InterruptedException {
					monitor.beginTask("Inferring FSM Model, Xinput, and Generating test cases", 5);
					// 0. prepare a temp folder
					
					if (performModelInfer || performXinputMining){
						monitor.subTask("Preparation...");
						if (!prepareLog()){
							informError("Ops, failed to prepare log files, please make sure they exist!");
							return;
						}
					}
					monitor.worked(1);			
					
					// 1. Infer FSM and Xinput
					if (performModelInfer){
						monitor.subTask("Infering model...");
						if (!inferFSM()){
							informError("Ops, failed to infer a model from logs!");
							return;
						}
					}
					monitor.worked(2);
					
					// 2. Infer input classifications
					if (performXinputMining){
						monitor.subTask("Mining domain input classifications...");
						if (!mineXInput()){
							informError("Ops, failed to infer domain input classifications!");
							return;
						}
					}
					monitor.worked(3);
					
					// 3. Generate CTE trees
					if (performCTEGen){
						monitor.subTask("Generating CT tree...");
						if (!generateCTETree()){
							informError("Ops, failed to generate classification trees from model!");
							return;
						}
					}
					monitor.worked(4);
					
					// 4. Generate tests 
					if (performTestGen){
						monitor.subTask("Generating Selenium test...");
						if (!generateTests()){
							informError("Ops, failed to generate tests from the generated classification tree!");
							return;
						}
					}
					monitor.done();
					// inform done
					
					shell.getDisplay().asyncExec(new Runnable() {
						@Override
						public void run() {
							MessageDialog.openInformation(shell, "Log 2 Test Wizard", "Model Inference and Test Generation completed!");
						}
					});
					
					try {
						selectedProject.refreshLocal(IResource.DEPTH_INFINITE, null);
					} catch (CoreException e) {
						e.printStackTrace();
					}
					
//					return new Status(IStatus.OK, "Log 2 Test Wizard", 
//									IStatus.OK, "Done", null);
				}
			};
			
			new ProgressMonitorDialog(shell).run(true, true, op);
			
			} catch (InvocationTargetException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (InterruptedException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			
//			Job generateJob = new Job("All in one Model inference and Test generation Job"){
//				protected IStatus run(final IProgressMonitor monitor) {				
			
					
//				}
//			};
//			generateJob.schedule();
		} else {
			informError("Make sure that a FITTEST project is selected and its project configuration file exist!");
			return false;
		}
		return true;
	}
	
	
	/**
	 * Inform the user the error
	 * @param message
	 */
	private void informError(final String message){
		shell.getDisplay().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openError(shell, "Log 2 Test Wizard", message);
			}
		});
	}


	/**
	 * Convert all the tests inside the ctes trees into java test cases
	 * This task might take longtime to run, all available processors will be used
	 * 
	 * @author cdnguyen
	 * @return
	 */
	private boolean generateTests() {
		String resource = projectConfig.getTestGeneration().getCteFolder();
		final String cteFolder =  ResourceUtils.getOSAbsolutePath(selectedProject, resource);
		if (cteFolder == null)
			return false;
		
		resource =  projectConfig.getModelInference().getDomainInputSpecFile();
		final String domainInputFile = ResourceUtils.getOSAbsolutePath(selectedProject, resource);
		if (domainInputFile == null)
			return false;
		
		final String packageName = projectConfig.getTestGeneration().getSourcePackagePrefix();
		final String outputFolder = selectedProjectRootDir + IFITTESTFolderConstants.TEST_SUITES ;
		final String targetPage = projectConfig.getGeneral().getEntryPage();
		final BrowserType seleniumDriver = projectConfig.getTestGeneration().getSeleniumDriverBrowser();
		final String seleniumDriverValue = projectConfig.getTestGeneration().getSeleniumDriverBrowser().value();
		final boolean onlyValidTestCase = true;
		
		File f = new File(cteFolder);

		final String[] fileList = f.list(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return name.endsWith(".cte");
			}
		});

		Bundle bundle = Platform.getBundle(eu.fittest.tranformtools.Activator.PLUGIN_ID);
		String template = null;
		
		try {
			if (seleniumDriver.equals(BrowserType.FLEX_OBJECT_DRIVER)) {
				template = FileLocator.toFileURL(bundle.getEntry("templates/junit.flexdriver.stg")).getFile();
			} else if (seleniumDriver.equals(BrowserType.FLASH_APPLICATION)) {
				template = FileLocator.toFileURL(bundle.getEntry("templates/junit.flashdriver.stg")).getFile();
			} else {
				template = FileLocator.toFileURL(bundle.getEntry("templates/junit4.wdriver.stg")).getFile();
			}
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		
		final String templateGroupFile = template;
			
		int i = 0;
		boolean finished = false;
		int numberOfThread = Runtime.getRuntime().availableProcessors();
		ExecutorService exec = Executors.newFixedThreadPool(numberOfThread);
		Future[] runningList = new Future[numberOfThread];
		Arrays.fill(runningList, null);

		while (!finished) {

			for (int j = 0; j < numberOfThread; j++) {
				if (i < fileList.length) {
					try {
						if (runningList[j] == null || runningList[j].get() == null) {
							final String s = fileList[i++];
							runningList[j] = exec.submit(new Runnable() {
								@Override
								public void run() {
									String className = s.replace(".cte", "").toUpperCase() + "Test";
									AbstractTemplateProvider templateProvider = new SeleniumDriverTemplateProvider(templateGroupFile);
									if (templateProvider.isTemplateReady()){
										try {	
												ITranformer transformer;
												if (seleniumDriver.equals(BrowserType.FLEX_OBJECT_DRIVER)) {
													transformer = new CTE2FlexSelenium(templateProvider, packageName, className, targetPage, seleniumDriverValue);
												} else if (seleniumDriver.equals(BrowserType.FLASH_APPLICATION)) {
													transformer = new CTE2FlashSelenium(templateProvider, packageName, className, targetPage, seleniumDriverValue);
												} else {
													transformer = new CTE2Selenium(templateProvider, packageName, className, targetPage, seleniumDriverValue);
												}
												String cteFile = cteFolder + File.separator + s;
												
												transformer.transform(cteFile,  domainInputFile, outputFolder, onlyValidTestCase);
										} catch (TransformException e) {
											e.printStackTrace();
										}
									}
								}
							});
						}
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ExecutionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
			
			if (i >= fileList.length) {
				finished = true;
				for (int j = 0; j < numberOfThread; j++) {
					try {
						if (runningList[j] != null && runningList[j].get() != null) {
							finished = false;
						}
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ExecutionException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		}
		
		// done, shutdown the executor
		exec.shutdown();
		
		return true;
	}


	/**
	 * Generate cte trees from inferred model
	 * 
	 * @return
	 */
	private boolean generateCTETree() {
		
		String resource = projectConfig.getModelInference().getModelFile();
		
		String fsmModel =  ResourceUtils.getOSAbsolutePath(selectedProject, resource); 
		// FSM must exist
		if (fsmModel == null) 
			return false;
				
		resource =  projectConfig.getModelInference().getDomainInputSpecFile();
		String domainInputs = ResourceUtils.getOSAbsolutePath(selectedProject, resource);
		
		// domain input must exist
		if (domainInputs == null)
			return false;
		
		boolean shouldGenerateTestCase = true;
		boolean shouldOptimizeTestCase = true;
		
		String visitorId  = getVisitorId(projectConfig.getTestGeneration().getModelVisitStrategy().value());
		String traceFolder = tmpFolder.toFile().getAbsolutePath();
		
		String outFolder = selectedProjectRootDir + projectConfig.getTestGeneration().getCteFolder();
		FSM2CTE transformer = new FSM2CTE(visitorId);
		
		transformer.transform(domainInputs, fsmModel, outFolder, shouldGenerateTestCase, shouldOptimizeTestCase, traceFolder);
		
		try {
			selectedProject.refreshLocal(IResource.DEPTH_INFINITE, null);
		} catch (CoreException e) {
			e.printStackTrace();
		}
		
		return true;
	}

	/**
	 * Get the correct visitor Id
	 * @param value
	 * @return
	 */
	private String getVisitorId(String value) {
		
		ModelVisitStrategyType type = ModelVisitStrategyType.fromValue(value);
		
		if (type.equals(ModelVisitStrategyType.VISITOR_BREADTHFIRST)){
			return VisitorFactory.VISITOR_BREADTHFIRST;
		}
		
		if (type.equals(ModelVisitStrategyType.VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS)) { 
		    return VisitorFactory.VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS)) { 
		    return VisitorFactory.VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_COVERAGE_UNIFORM)) { 
		    return VisitorFactory.VISITOR_COVERAGE_UNIFORM;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_SEQ_MAXK)) { 
		    return VisitorFactory.VISITOR_SEQ_MAXK;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_SEQK)) { 
		    return VisitorFactory.VISITOR_SEQK;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_SEMK)) { 
		    return VisitorFactory.VISITOR_SEMK;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_SEM_MAXK)) { 
		    return VisitorFactory.VISITOR_SEM_MAXK;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_SE_MSEXTRACTOR_ONLY_LAST_EVENT_MAX_K)) { 
		    return VisitorFactory.VISITOR_SEMsextractor_onlyLastEvent_maxK;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_SE_MSEXTRACTOR_ONLY_LAST_EVENT_K)) { 
		    return VisitorFactory.VISITOR_SEMsextractor_onlyLastEvent_K;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY)) { 
		    return VisitorFactory.VISITOR_DIVERSITY;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_TC)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_TC;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_TL)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_TL;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_EDM)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_EDM;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_EDA)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_EDA;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_ONLY_LAST_EVENT_TC)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_onlyLastEvent_TC;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_ONLY_LAST_EVENT_TL)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_onlyLastEvent_TL;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_ONLY_LAST_EVENT_EDA)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_onlyLastEvent_EDA;
		}
		if (type.equals(ModelVisitStrategyType.VISITOR_DIVERSITY_ONLY_LAST_EVENT_EDM)) { 
		    return VisitorFactory.VISITOR_DIVERSITY_onlyLastEvent_EDM;
		}
					
		// default
		return VisitorFactory.VISITOR_BREADTHFIRST;
	}

	
	/**
	 * translate the driver id
	 * @param value
	 * @return
	 */
	/*
	private String getSelenDriverId(String value){
		
		BrowserType type = BrowserType.fromValue(value);
		
		if (type.equals(BrowserType.HTML_UNIT_DRIVER.value())){
		    return Constants.HTML_UNIT_DRIVER;
		}
		if (type.equals(BrowserType.FIREFOX_DRIVER)){
		    return Constants.HTML_FIREFOX_DRIVER;
		}
		if (type.equals(BrowserType.CHROME_DRIVER)){
		    return Constants.HTML_CHROME_DRIVER;
		}
		if (type.equals(BrowserType.INTERNET_EXPLORER_DRIVER)){
		    return Constants.HTML_IE_DRIVER;
		}
		if (type.equals(BrowserType.FLEX_OBJECT_DRIVER)){
		    return Constants.FLEX_OBJECT_DRIVER;
		}
		if (type.equals(BrowserType.FLASH_APPLICATION)){
		    return Constants.FLASH_OBJECT_DRIVER;
		}
		// default 
		return value;
	}
	*/
	/**
	 * Minining domain input classification job
	 * @return
	 */
	private boolean mineXInput() {
		
		String resource = projectConfig.getLogging().getLogTarget().getStoreDir();
		
		String logFolder =  ResourceUtils.getOSAbsolutePath(selectedProject, resource); 
		// Log folder must exist
		if (logFolder == null) 
			return false;

		resource = projectConfig.getModelInference().getModelFile();
		String fsmModel = ResourceUtils.getOSAbsolutePath(selectedProject, resource);
		// FSM model must exist
		if (fsmModel == null)
			return false;
		
		String domainInputs = selectedProjectRootDir + projectConfig.getModelInference().getDomainInputSpecFile();
		
		XinputMiner miner = new XinputMiner();
		miner.mine(fsmModel, logFolder, domainInputs);
		
		//refresh new file
		try {
			selectedProject.refreshLocal(IResource.DEPTH_INFINITE, null);
		} catch (CoreException e) {
			e.printStackTrace();
		}
		return true;
				
	}


	/**
	 * Infer FSM model from the input log using the chosen inference technique
	 * 
	 * @return
	 */
	private boolean inferFSM() {
		String outputFSM = selectedProjectRootDir + projectConfig.getModelInference().getModelFile();
		InferenceTechniqueType inferenceAlgo = projectConfig.getModelInference().getInferenceTechnique();
		
		// State-based model inference 
		if (inferenceAlgo.equals(InferenceTechniqueType.STATE_BASED)){
			String activeInputFolder = LogFileUtils.convert2FBKFormat(tmpFolder, tmpFolder);
			if (activeInputFolder == null){
				return false;
			}
			
			FSMmanager fsmmanager = new FSMmanager();
			Config_Launcher cl = Config_Launcher.getInstace();
			
			File[] fileList=fsmmanager.getFilelist(activeInputFolder, cl.maxFilePemutations);
//			File[] fileCurrent=new File[1];
//			
//			// infering model
//			for (int logIndex = 0; logIndex < fileList.length; logIndex++) {
//				fileCurrent[0]=fileList[logIndex];
//			
//				if (logIndex==0) fsmmanager.generateFSM(fileCurrent, true, 0);
//				else fsmmanager.generateFSM(fileCurrent, false, 0);
//			}
//			
			// infer model
			fsmmanager.generateFSM(fileList, false, 0);

			// saving model
			fsmmanager.FSM2FsmXpr(outputFSM, null, null);
			
			// saving DOT file
			String dotFileName = outputFSM.replace(".fsm", ".dot");
			fsmmanager.FSM2DOT(dotFileName);
		} else if (inferenceAlgo.equals(InferenceTechniqueType.SEQUENCE_BASED)){
			// sequence-based algorithm
			try {
				EventBasedFSM.computeFSMfromTraces(tmpFolder.toOSString(), outputFSM, "xml");
			} catch (Exception e) {
				e.printStackTrace();
				return false;
			}
		} else if (inferenceAlgo.equals(InferenceTechniqueType.AUTO_ABS)){
			// TODO: Cu to implement automated abstraction inference
			
		}
		
		try {
			selectedProject.refreshLocal(IResource.DEPTH_INFINITE, null);
		} catch (CoreException e) {
			e.printStackTrace();
		}
		
		return true;
		
	}
	
	/**
	 * Aggregate all logs in to the temporary folder
	 * @return
	 */
	private boolean prepareLog(){
		
		// prepare temp folder
		tmpFolder = LogFileUtils.prepareTempFolder();
		
		String resource = projectConfig.getLogging().getLogTarget().getStoreDir();
		String logFolder =  ResourceUtils.getOSAbsolutePath(selectedProject, resource); 
				
		// Log folder must exist
		if (logFolder == null) 
			return false;
		
		IFolder res = selectedProject.getFolder(resource);
		
		try {
			List<IFile> allLogFiles = ResourceUtils.collectFiles(res, "xml", "log_");
			if (allLogFiles.size() == 0)
				return false;
			
			// Temporary copy all the log files into a single folder
			for (IFile logFile : allLogFiles){
				LogFileUtils.copyFile(logFile, tmpFolder);
			}
		} catch (CoreException e) {
			return false;
		}
		
		return true;
	}

	
	
	@Override
	public void addPages() {
		ImageDescriptor imgDes = eu.fittest.tranformtools.Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");

		selectionPage = new AllInOneLog2TestPage("Log 2 Test: All in One Wizard", projectConfig);
		selectionPage.setImageDescriptor(imgDes);
		selectionPage.setDescription("Select tasks to perform:");
		
		addPage(selectionPage);
	}
	
	
}
