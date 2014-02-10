package eu.fittest.eclipse.allinonegui.views;


import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.wb.swt.ResourceManager;
import org.eclipse.wb.swt.SWTResourceManager;

import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.wizards.recordingsession.FITTESTRecordingSessionWizard;
import eu.fittest.eclipse.gui.wizards.recordingsession.FITTESTRecordingSessionWizardPageTwo;
import eu.fittest.eclipse.gui.wizards.replayingsession.FITTESTReplayingSessionWizard;
import eu.fittest.eclipse.testproject.formeditor.TestProjectEditor;
import eu.fittest.tranformtools.wizards.AllInOneLog2TestWizard;


public class AllInOneView extends ViewPart {

	public static final String ID = "eu.fittest.eclipse.allinonegui.views.AllInOneView";
	private Label logTitle;
	private Label tGenTitle;
	private Text tExecTitle;
	private Label logConfig;
	private Label logShow;
	private Label tGenConfig;
	private Label tGenShow;
	private Label tExecConf;
	private Label tExecShow;
	private ToolBar logBar;
	private ToolItem logRecord;
	private ToolItem logPause;
	private ToolItem logStop;
	private ToolBar tExecBar;
	private ToolItem tExecExec;
	private ToolItem tExecPause;
	private ToolItem tExecStop;
	private ToolBar tGenBar;
	private ToolItem tGenGen;
	private Label arrowLabel1;
	private Label arrowLabel2;
	private Label spiderLabel;
	private ProgressBar tGenProgress;
	private ProgressBar tExecProgress;
	private Label logStatus;
	private String loggingSession = "";

	private String logRoot = "Testing sessions";
	private String modelRoot = "Models";
	private String testSuiteRoot = "Test suites";

	private FileDialog dialog = null;

	/**
	 * The constructor.
	 */
	public AllInOneView() {
	}


	/**
	 * This method launch the folder browsing
	 */

	public void searchFile() {

	}


	public void createPartControl(Composite parent) {
		parent.setLayout(new GridLayout(3, true));

		boolean beautifulBgr = false;

		if (Platform.getOS().equals(Platform.OS_MACOSX) 
				|| Platform.getOS().equals(Platform.OS_LINUX))
			beautifulBgr = true;

		Composite loggingComposite = new Composite(parent, SWT.NONE);
		loggingComposite.setLayout(new GridLayout(2, false));
		GridData gd_loggingComposite = new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1);
		gd_loggingComposite.minimumWidth = 200;
		gd_loggingComposite.minimumHeight = 100;
		loggingComposite.setLayoutData(gd_loggingComposite);
		if (beautifulBgr)
			loggingComposite.setBackgroundImage(ResourceManager.getPluginImage("eu.fittest.eclipse.allinonegui", "icons/bgr.png"));
		else 
			loggingComposite.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));


		logTitle = new Label(loggingComposite, SWT.NONE);
		logTitle.setFont(SWTResourceManager.getFont("Segoe UI", 12, SWT.BOLD));
		logTitle.setText("Logging");
		logTitle.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		if (!beautifulBgr)
			logTitle.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		logStatus = new Label(loggingComposite, SWT.NONE);
		logStatus.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showLogStatus();
			}
		});
		logStatus.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		logStatus.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false, 1, 1));
		logStatus.setText("Status");
		if (!beautifulBgr)
			logStatus.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		logConfig = new Label(loggingComposite, SWT.FLAT);
		logConfig.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showLogConf();
			}
		});
		logConfig.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		logConfig.setText("config");
		logConfig.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false, 1, 1));

		if (!beautifulBgr)
			logConfig.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		logShow = new Label(loggingComposite, SWT.NONE);
		logShow.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showLogResults();
			}
		});
		logShow.setEnabled(true);
		logShow.setText("show");
		logShow.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		logShow.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		if (!beautifulBgr)
			logShow.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		new Label(loggingComposite, SWT.NONE);
		new Label(loggingComposite, SWT.NONE);

		logBar = new ToolBar(loggingComposite, SWT.PUSH | SWT.RIGHT);
		logBar.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));
		if (!beautifulBgr)
			logBar.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		logRecord = new ToolItem(logBar, SWT.PUSH);
		logRecord.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				loggingSession = startLog();
			}
		});
		logRecord.setText("Logging Wizard");


		spiderLabel = new Label(parent, SWT.NONE);
		spiderLabel.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 1, 1));
		spiderLabel.setImage(ResourceManager.getPluginImage("eu.fittest.eclipse.allinonegui", "icons/fittest-logo-new-transparent-small.png"));

		Composite testExecComposite = new Composite(parent, SWT.NONE);
		testExecComposite.setLayout(new GridLayout(2, false));
		GridData gd_testExecComposite = new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1);
		gd_testExecComposite.widthHint = 166;
		gd_testExecComposite.minimumWidth = 200;
		gd_testExecComposite.minimumHeight = 100;
		testExecComposite.setLayoutData(gd_testExecComposite);

		if (!beautifulBgr)
			testExecComposite.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));
		else 
			testExecComposite.setBackgroundImage(ResourceManager.getPluginImage("eu.fittest.eclipse.allinonegui", "icons/bgr.png"));


		tExecTitle = new Text(testExecComposite, SWT.NONE);
		tExecTitle.setFont(SWTResourceManager.getFont("Segoe UI", 12, SWT.BOLD));
		tExecTitle.setText("Test Execution");
		tExecTitle.setEditable(false);
		tExecTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

		if (!beautifulBgr)
			tExecTitle.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tExecConf = new Label(testExecComposite, SWT.NONE);
		tExecConf.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showTExecConf();
			}
		});
		tExecConf.setText("config");
		tExecConf.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		tExecConf.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false, 1, 1));

		if (!beautifulBgr)
			tExecConf.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tExecShow = new Label(testExecComposite, SWT.NONE);
		tExecShow.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showTExecResult();
			}
		});
		tExecShow.setEnabled(true);
		tExecShow.setText("show");
		tExecShow.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		tExecShow.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, true, false, 1, 1));
		if (!beautifulBgr)
			tExecShow.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tExecProgress = new ProgressBar(testExecComposite, SWT.NONE);
		tExecProgress.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));
		if (!beautifulBgr)
			tExecProgress.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tExecBar = new ToolBar(testExecComposite, SWT.PUSH | SWT.RIGHT);
		tExecBar.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));

		tExecExec = new ToolItem(tExecBar, SWT.NONE);
		tExecExec.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				startTExec();
			}
		});
		tExecExec.setText("Execution Wizard");

		//		tExecPause = new ToolItem(tExecBar, SWT.CHECK);
		//		tExecPause.setEnabled(false);
		//		tExecPause.addSelectionListener(new SelectionAdapter() {
		//			@Override
		//			public void widgetSelected(SelectionEvent e) {
		//				pauseTExec();
		//			}
		//		});
		//		tExecPause.setText("Pause");
		//		
		//		tExecStop = new ToolItem(tExecBar, SWT.NONE);
		//		tExecStop.setEnabled(false);
		//		tExecStop.addSelectionListener(new SelectionAdapter() {
		//			@Override
		//			public void widgetSelected(SelectionEvent e) {
		//				stopTExec();
		//			}
		//		});
		//		tExecStop.setText("Stop");

		arrowLabel1 = new Label(parent, SWT.NONE);
		arrowLabel1.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_HIGHLIGHT_SHADOW));
		arrowLabel1.setImage(ResourceManager.getPluginImage("eu.fittest.eclipse.allinonegui", "icons/arrow1.png"));
		arrowLabel1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1));

		Composite testGenComposite = new Composite(parent, SWT.NONE);
		testGenComposite.setLayout(new GridLayout(2, false));
		GridData gd_testGenComposite = new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1);
		gd_testGenComposite.minimumWidth = 200;
		gd_testGenComposite.minimumHeight = 100;
		testGenComposite.setLayoutData(gd_testGenComposite);
		if (!beautifulBgr)
			testGenComposite.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));
		else
			testGenComposite.setBackgroundImage(ResourceManager.getPluginImage("eu.fittest.eclipse.allinonegui", "icons/bgr.png"));


		tGenTitle = new Label(testGenComposite, SWT.NONE);
		tGenTitle.setFont(SWTResourceManager.getFont("Segoe UI", 12, SWT.BOLD));
		tGenTitle.setText("Test Generation");
		tGenTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		if (!beautifulBgr)
			tGenTitle.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tGenConfig = new Label(testGenComposite, SWT.NONE);
		tGenConfig.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showTGenConf();
			}
		});
		tGenConfig.setText("config");
		tGenConfig.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		tGenConfig.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, true, false, 1, 1));
		if (!beautifulBgr)
			tGenConfig.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tGenShow = new Label(testGenComposite, SWT.NONE);
		tGenShow.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseUp(MouseEvent e) {
				showTGenResult();
			}

		});
		tGenShow.setEnabled(true);
		tGenShow.setText("show");
		tGenShow.setForeground(SWTResourceManager.getColor(SWT.COLOR_BLUE));
		tGenShow.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		if (!beautifulBgr)
			tGenShow.setBackground(SWTResourceManager.getColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));

		tGenProgress = new ProgressBar(testGenComposite, SWT.NONE);
		tGenProgress.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));
		tGenBar = new ToolBar(testGenComposite, SWT.PUSH | SWT.RIGHT);
		tGenBar.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));

		tGenGen = new ToolItem(tGenBar, SWT.NONE);
		//		tGenGen.setEnabled(false);
		tGenGen.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				startTGen();
			}
		});
		tGenGen.setText("Generation Wizard");

		arrowLabel2 = new Label(parent, SWT.NONE);
		arrowLabel2.setImage(ResourceManager.getPluginImage("eu.fittest.eclipse.allinonegui", "icons/arrow2.png"));		

		this.dialog = new FileDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), SWT.OPEN);

	}

	@Override
	public void setFocus() {
	}

	/**
	 * Show available clients
	 */
	protected void showLogStatus() {
		try {
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView("eu.fittest.eclipse.gui.views.agent");
		} catch (PartInitException e) {
			MessageDialog.openInformation(logConfig.getShell(), "Information", "Could not open the FITTEST Remote Agents view");
			e.printStackTrace(System.err);
		}
	}

	/**
	 * Show log configuration
	 */
	protected void showLogConf() {
		if (!openProjectConfiguration(1)){
			MessageDialog.openInformation(logConfig.getShell(), "Information", "Could not open any project configuration, please make sure that you have selected\n"
					+ "a FITTEST test project in the project view.");
		}
	}


	/**
	 * Show log results
	 */
	protected void showLogResults() {
		IProject selectedProject = Activator.getDefault().getActiveProject();
		if (selectedProject != null) {
			IFolder testingFolder = selectedProject.getFolder(this.logRoot);
			IFolder logFolder = testingFolder.getFolder(this.loggingSession);
			if (logFolder.exists()){
				showResults(this.logRoot + File.separator + this.loggingSession);
			}else{
				showResults(this.logRoot);
			}
		}
	}

	/**
	 * Open folder 
	 */
	private void showResults(String rootPath) {
		IProject selectedProject = Activator.getDefault().getActiveProject();
		if (selectedProject != null) {
			IFolder testingFolder = selectedProject.getFolder(rootPath);

			String logFolderPath = testingFolder.getLocation().toOSString();
			File logFolderFile = new File(logFolderPath);

			if ((testingFolder != null) && (logFolderFile.exists())){
				this.dialog.setFilterPath(logFolderPath);
				String filePath = this.dialog.open();
				IPath project = selectedProject.getLocation();
				IFile fileToBeOpened = selectedProject.getFile(filePath.replace(project.toOSString(), ""));
				
			
				IEditorInput editorInput = new FileEditorInput(fileToBeOpened);
				IWorkbenchWindow window=PlatformUI.getWorkbench().getActiveWorkbenchWindow();
				IWorkbenchPage page = window.getActivePage();
				IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(fileToBeOpened.getName());
				try {
					if (desc != null)
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(fileToBeOpened), desc.getId());
					else{
						page.openEditor(editorInput, "org.eclipse.ui.DefaultTextEdtior");
					}
				} catch (PartInitException e) {

					e.printStackTrace(System.err);
				} 

				//				IFile fileToOpen = testingFolder.getFile(filePath);
				//
				//				// open with a default editor
				//				IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(fileToOpen.getName());
				//				if (desc != null)
				//					try {
				//						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(fileToOpen), desc.getId());
				//					} catch (PartInitException e) {
				//						e.printStackTrace(System.err);
				//					}

			}else{
				MessageDialog.openInformation(logConfig.getShell(), "Information", "Could not open any log folder.");
			}
		}else{
			MessageDialog.openInformation(logConfig.getShell(), "Information", "Could not open any project configuration, please make sure that you have selected\n"
					+ "a FITTEST test project in the project view.");
		}

	}

	/**
	 * Start recording of logs
	 */
	protected String startLog() {

		String loggingSessionName = "";
		IProject selectedProject = Activator.getDefault().getActiveProject();
		if (selectedProject != null){
			FITTESTRecordingSessionWizard wizard = new FITTESTRecordingSessionWizard();
			wizard.init(PlatformUI.getWorkbench(), selectedProject);
			WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
			dialog.open();
			loggingSessionName = ((FITTESTRecordingSessionWizardPageTwo) wizard.getPages()[0]).getSessionName();
		} else {
			askToSelectAProject(logTitle.getShell());


		}
		return loggingSessionName;

	}

	/**
	 * Pause / continue recording of logs 
	 */
	protected void pauseLog() {
		logRecord.setEnabled(false);
		logPause.setEnabled(true);
		logStop.setEnabled(true);	
	}

	/**
	 * Stop recording of logs
	 */
	protected void stopLog() {
		logRecord.setEnabled(true);
		logPause.setEnabled(false);
		logPause.setSelection(false);
		logStop.setEnabled(false);
		logShow.setEnabled(true);
		tGenGen.setEnabled(true);
	}

	/**
	 * Show test execution configuration
	 */
	protected void showTExecConf() {
		if (!openProjectConfiguration(3)){
			askToSelectAProject(tGenConfig.getShell());
		} 
	}

	/**
	 * Show test execution results
	 */
	protected void showTExecResult() {
		showResults(this.logRoot);
	}

	/**
	 * Start execution of tests
	 */
	protected void startTExec() {
		tExecProgress.setSelection(tExecProgress.getMinimum());
		tExecProgress.setSelection(tExecProgress.getMaximum());
		/*
		 * here some call-back mechanism is needed to get the actual
		 * progress of test execution ...
		 */

		//		tExecExec.setEnabled(false);
		//		tExecPause.setEnabled(true);
		//		tExecStop.setEnabled(true);

		IProject selectedProject = Activator.getDefault().getActiveProject();
		if (selectedProject!= null){
			FITTESTReplayingSessionWizard wizard = new FITTESTReplayingSessionWizard();
			wizard.init(PlatformUI.getWorkbench(), selectedProject);
			WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
			dialog.open();
		} else {
			askToSelectAProject(tExecTitle.getShell());
		}

	}

	/**
	 * Pause / continue execution of tests 
	 */
	protected void pauseTExec() {
		tExecExec.setEnabled(false);
		tExecPause.setEnabled(true);
		tExecStop.setEnabled(true);

		tExecProgress.setState(tExecProgress.getState() == SWT.NORMAL ? SWT.PAUSED
				: SWT.NORMAL);
	}

	/**
	 * Stop execution of tests
	 */
	protected void stopTExec() {
		tExecExec.setEnabled(true);
		tExecPause.setEnabled(false);
		tExecPause.setSelection(false);
		tExecStop.setEnabled(false);
		tExecShow.setEnabled(true);

		tExecProgress.setState(SWT.NORMAL);

	}

	/**
	 * Show test generation configuration
	 */
	protected void showTGenConf() {
		if (!openProjectConfiguration(2)){
			askToSelectAProject(tGenConfig.getShell());
		} 
	}

	/**
	 * Show test generation results
	 */
	protected void showTGenResult() {
		showResults(this.modelRoot);
	}

	/**
	 * Start test generation
	 */
	protected void startTGen() {
		tGenProgress.setSelection(tExecProgress.getMinimum());
		tGenProgress.setSelection(tGenProgress.getMaximum());
		/*
		 * here some call-back mechanism is needed to get the actual
		 * progress of test generation ...
		 */

		//		tGenShow.setEnabled(true);
		//		tExecExec.setEnabled(true);

		IProject selectedProject = Activator.getDefault().getActiveProject();
		if (selectedProject!= null){
			AllInOneLog2TestWizard wizard = new AllInOneLog2TestWizard();
			wizard.init(this.getSite().getWorkbenchWindow().getWorkbench(), selectedProject);
			WizardDialog dialog = new WizardDialog(this.getSite().getWorkbenchWindow().getShell(), wizard);
			dialog.open();
		} else {
			askToSelectAProject(tExecTitle.getShell());
		}

	}

	/**
	 * Open 
	 * @param pageIndex
	 * @return
	 */
	private boolean openProjectConfiguration(int pageIndex){
		IProject activeProject = Activator.getDefault().getActiveProject();
		if (activeProject != null){
			IFile projectConfigFile = activeProject.getFile("fittest.project");
			if (!projectConfigFile.exists()){
				return false;
			}
			IEditorDescriptor[] descs =  PlatformUI.getWorkbench().getEditorRegistry().getEditors(projectConfigFile.getName()); //PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(projectConfigFile.getName());

			try {
				for (IEditorDescriptor editorDesc : descs){
					if (editorDesc.getId().equals("eu.fittest.eclipse.testproject.formeditor.ProjectEditor")){
						IEditorPart editor = getViewSite().getWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(projectConfigFile), editorDesc.getId());

						// Select the associated page
						TestProjectEditor projectEditor = (TestProjectEditor)editor;
						projectEditor.setPage(pageIndex);
						return true;
					}
				}
			} catch (PartInitException e) {
				e.printStackTrace();
			}
		}
		return false;
	}

	/**
	 * Inform the user to select a fittest project 
	 * @param shell
	 */
	private void askToSelectAProject(Shell shell){
		MessageDialog.openInformation(shell, "Information", "Could not open any project configuration, please make sure that you have selected\n"
				+ "a FITTEST test project in the project view and it contains the fittest.project file.");
	}

}