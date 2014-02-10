package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.service.log.LogService;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.plugin.handlers.Efsm2CtAbstractHandler;
import eu.fittest.fbk.efsm2ct.plugin.tool.AdaptationStep;
import eu.fittest.fbk.efsm2ct.plugin.utils.GuiHelper;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.plugin.utils.TransitionsCoverageChecker;
import eu.fittest.fbk.efsm2ct.plugin.utils.TransitionsTracker;
import eu.fittest.fbk.efsm2ct.plugin.utils.TransitionsTracker.Triple;
import eu.fittest.fbk.efsm2ct.plugin.views.LogView;
import eu.fittest.fbk.efsm2ct.tools.evosuite.EvosuiteService;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessSpawnException;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * 
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class EvosuiteRunHandler extends Efsm2CtAbstractHandler {

	// protected static Logger logger = Logger.getAnonymousLogger();

	private static final String FITTEST_USER_LIBS = "FITTEST_libs"; // TODO is
																	// there
																	// another
																	// place
																	// where
																	// this is
																	// defined?

	private LogView logView;
	private boolean adaptationStepToBePerformed;
	private boolean debug = true;

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 * 
	 * @throws ExecutionException
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		ISelection mySelection = HandlerUtil.getCurrentSelection(event);
		Shell myShell = HandlerUtil.getActiveShell(event);
		logView = lookForLogView();

		if (logView == null) {

			Activator
					.getDefault()
					.osgiLog(LogService.LOG_ERROR,
							"Error running evosuite, can't get a LogView instance, abort execution");
			throw new ExecutionException(
					"can't get a LogView instance, abort execution");

		}
		
		logView.clear();

		// logger.info("run:" + event);

		if (!mySelection.isEmpty()) {

			StructuredSelection selection = (StructuredSelection) mySelection;

			org.eclipse.core.internal.resources.Folder fe = (org.eclipse.core.internal.resources.Folder) selection
					.getFirstElement();

			IProject project = fe.getProject();

			IFolder outputFolder = (IFolder) fe.findMember("output");

			List<IResource> matched;

			try {
				matched = ResourceUtils.match(outputFolder, "^.*\\.efsm$");
			} catch (CoreException e1) {
				throw new ExecutionException("internal error", e1);
			}

			if (matched.size() != 1) {
				throw new ExecutionException(
						"internal error: invalid number of resource selected:"
								+ matched.size());
			}

			File fsmFile = matched.get(0).getLocation().toFile();

			IFolder binFolder = project.getFolder("/bin");

			try {

				String packageNamePrefix = fe.getName();

				String packagePathStr = packageNamePrefix.replace(".",
						File.separator);

				String sutName = fsmFile.getName().replace(".efsm", "");

				// IFolder libFolder = project.getFolder("/lib");

				File projectDir = project.getLocation().toFile();

				// File libDir = libFolder.getLocation().toFile();
				File binDir = binFolder.getLocation().toFile();

				TransitionsTracker tt = new TransitionsTracker(
						fsmFile.getAbsolutePath());

				String classQualifiedName = packageNamePrefix + "." + "Test"
						+ sutName;

				// String sessionName = sessionFolder.getName();
				IFolder evoTestsFolder = outputFolder
						.getFolder("evosuite-tests");

				IFolder testSuitesDirectory = project
						.getFolder(IFITTESTFolderConstants.TEST_SUITES);

				String txlTransformation = "transform_evotest_to_selenium.txl";

				String additionalMethodFile = "additional_methods_selenium.java";

				File txlTransformationDir = new File(
						lookForEvosuiteBundlePath("resources"), "txl");
				
				IFolder sessionFolder = (IFolder) selection.getFirstElement();
				
				 String sessionName = sessionFolder.getName();

				scheduleRunEvosuite(packageNamePrefix, classQualifiedName,
						project, outputFolder.getLocation().toFile(), binDir,
						tt, outputFolder, txlTransformationDir,  txlTransformation, additionalMethodFile, sessionName
						);

			} catch (Exception e) {

				Activator.getDefault().osgiLog(LogService.LOG_ERROR,
						"Error running evosuite", e);
				GuiHelper.showError(myShell, "can't run evosuite", e);

			}

		}
		return null;

	}

	/*
	 * EXAMPLE java -jar
	 * /home/tiella/local_SE/src/evosuite_tiella/target/evosuite
	 * -0.1-SNAPSHOT-jar-minimal.jar -cp :dist/lib/junit-4.10.jar:dist/lib
	 * /selenium-server-standalone-2.31.0 .jar:dist/lib/eu.fittest.common-
	 * 1.1.0-SNAPSHOT.jar:dist/lib/fsmtester .jar:dist/lib/flashselenium-java
	 * -client-extension-1.0.jar:dist/flexfsmtest.jar -generateSuite -class
	 * eu.fittest.sbtest.generated.TestNewFlexstore6 -Dshow_progress=false
	 * -Dstartup_method=_startup -Dshutdown_method=_shutdown -Dpopulation=15
	 * -Dmake_accessible=false -Dsearch_budget=800000 -Dminimize=true
	 * -Dlog_timeout=true -Dtimeout=300000 -Dglobal_timeout=800000
	 * -Dchromosome_length=30 -Dminimization_timeout=800000
	 * -Dprint_to_system=true
	 */

	private void runEvosuite(String packagePrefix, String classQualifiedName,
			File workingDir, File binDir, TransitionsCoverageChecker checker)
			throws IOException, ProcessSpawnException, URISyntaxException,
			ExecutionException {

		boolean okToRun = false;

		String libPath = lookForEvosuiteBundlePath("lib");
		File libDir = new File(libPath);

		EvosuiteService es = new EvosuiteService(classQualifiedName);

		es.setEvosuiteDirPath(libPath);
		es.setWorkingDirectory(workingDir.getAbsolutePath());
		es.setEvosuiteJarName("evosuite-minimal.jar");
		es.addClasspath(binDir.getAbsolutePath());

		String[] libs = new String[] { "eu.fittest.common.jar",
				"eu.fittest.fbk.efsm2ct.efsm2mon.jar",
				"eu.fittest.fbk.efsm2ct.flexdrv.jar", "evosuite-minimal.jar",
				"flashselenium-java-client-extension.jar", "guava.jar",
				"selenium-api.jar", "selenium-chrome-driver.jar",
				"selenium-java.jar", "selenium-remote-driver.jar"

		};

		for (String l : libs) {
			es.addClasspath(new File(libDir, l).getAbsolutePath());
		}

		// mandatory property values
		es.addProperty(EvosuiteService.shutdown_method, "_shutdown");
		es.addProperty(EvosuiteService.startup_method, "_startup");

		// GA related
		es.addProperty(EvosuiteService.population, Integer
				.toString(ConfigurationFactory.getInstance()
						.getEvosuitePopulationSize()));
		es.addProperty(EvosuiteService.chromosome_length, Integer
				.toString(ConfigurationFactory.getInstance()
						.getEvosuiteChromosomeLength()));

		// 1100 is a value that can be required
		es.addProperty("max_int", Integer.toString(1200)); // TODO a constant
															// string must be
															// created into
															// EvosuiteService

		// search process timings
		es.addProperty(EvosuiteService.timeout, "300000");
		
		es.addProperty(EvosuiteService.search_budget, Integer
				.toString(ConfigurationFactory.getInstance().getEvosuiteGenerations()));
		
		es.addProperty(EvosuiteService.global_timeout, "60000"); // seconds
		es.addProperty(EvosuiteService.minimization_timeout, "800000"); // almost
																		// infinite

		es.addProperty(EvosuiteService.report_dir, "evosuite-reports/"
				+ packagePrefix.replace('.', File.separatorChar));

		// other properties
		es.addProperty(EvosuiteService.stopping_port, Integer
				.toString(ConfigurationFactory.getInstance()
						.getEvosuiteStoppingPort()));
		es.addProperty(EvosuiteService.make_accessible, "false");
		es.addProperty(EvosuiteService.print_to_system, "true");

		if (checker != null) {

			es.setKillChecker(checker);

		}

		// register the consumer for the output
		if (logView != null) {
			es.addLogConsumer(logView);
			okToRun = true;
		}

		if (okToRun) {
			es.init();
			es.run();
		}

	}

	private void scheduleRunEvosuite(final String packagePrefix, final String classQualifiedName, 
			final IProject project, final File workingDir, final File binDir, final TransitionsTracker tt,
			final IFolder outputFolder, final File txlTransformationDir, final String txlTransformation, final String additionalMethodFile, final String sessionName
			) {
		
		final int evosuteShutdownPort = ConfigurationFactory.getInstance().getEvosuiteStoppingPort();
		
		// String sessionName = sessionFolder.getName();
		final IFolder evoTestsFolder = outputFolder.getFolder("evosuite-tests");

		final IFolder testSuitesDirectory = project
				.getFolder(IFITTESTFolderConstants.TEST_SUITES);
		
		final int popSize = ConfigurationFactory.getInstance().getEvosuitePopulationSize();
		final int chrmLen = ConfigurationFactory.getInstance().getEvosuiteChromosomeLength();
		final int genrMax = ConfigurationFactory.getInstance().getEvosuiteGenerations();
			
		if (debug ) {
			logView.scheduleTextUpdate("popSize:"+popSize+"\n");
			logView.scheduleTextUpdate("chrmLen:"+chrmLen+"\n");
			logView.scheduleTextUpdate("genrMax:"+genrMax+"\n");
		}
		

		Job generateJob = new Job("Generate Test Cases") {

			@Override
			protected IStatus run(final IProgressMonitor monitor) {

				TransitionsCoverageChecker checker = new TransitionsCoverageChecker(tt, evosuteShutdownPort, monitor, logView);

				monitor.beginTask("Generating test cases...", tt.size());

				try {
					// TODO how to show logView?
					// logView.getSite().getWorkbenchWindow().getActivePage().showView("eu.fittest.fbk.efsm2ct.plugin.views.log");

					// showView("eu.fittest.fbk.efsm2ct.plugin.views.log");

					logView.setFocus();
					logView.showBusy(true);
					
					long startTime = System.currentTimeMillis();
					
					runEvosuite(packagePrefix, classQualifiedName, workingDir, binDir, checker);
					
					long endTime = System.currentTimeMillis();
					
					logView.showBusy(false);
					monitor.done();
					
					logView.setFocus();
					
					StringBuilder reportBuilder = new StringBuilder();
					
					createReport(reportBuilder,tt,startTime,endTime,popSize,chrmLen,genrMax);
					
					BufferedWriter bw = new BufferedWriter(new FileWriter(new File(workingDir, "report.txt")));
					bw.append(reportBuilder.toString());
					bw.close();
					
					logView.scheduleTextUpdate(reportBuilder.toString());
					
					logView.scheduleTextUpdate("Run terminated");
					
					refresh(project);
					
					if (isAdaptationStepToBePerformed()) {
						
						int status = AdaptationStep.performAdaptation(
								txlTransformationDir, sessionName, evoTestsFolder,testSuitesDirectory, txlTransformation,
								additionalMethodFile);
						
						refresh(project);
						
						
						if (status != 0)  {
									
							return new Status(IStatus.ERROR, "eu.fittest.fbk.efsm2ct.plugin", IStatus.OK, "Error Generating Test Suites!", null);
						}
						
					}
					
					refresh(project);
					
					return new Status(IStatus.OK, "eu.fittest.fbk.efsm2ct.plugin", IStatus.OK, "Finish Generating Test Suites!", null);

				} catch (Exception e) {
					// TODO Auto-generated catch block
					Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Error running evosuite", e);
					logView.showBusy(false);
					monitor.done();
					try {
						refresh(project);
					} catch (ExecutionException e1) {
						// TODO Auto-generated catch block
						Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Error refreshing the project", e1);
					}
					return new Status(IStatus.ERROR, "eu.fittest.fbk.efsm2ct.plugin", IStatus.OK, "Error Generating Test Suites!", e);

				}

			}

		};

		generateJob.schedule();

	}

	private void createReport(StringBuilder buffer,
			final TransitionsTracker tt, final long stime, final long etime, final int popSize, final int chrmLen, final int genrMax) {
		

		
		buffer.append(String.format("pop size:%d\n",popSize));
		buffer.append(String.format("chrm len:%d\n",chrmLen));
		buffer.append(String.format("genr max:%d\n",genrMax));

		Map<Triple<String, String, String>, Boolean> targets = tt.getTargets();

		int covered = 0;

		buffer.append("covered transitions:\n");

		for (Triple<String, String, String> k : targets.keySet()) {

			if (targets.get(k)) {
				covered++;
				buffer.append(k.getC1() + " " + k.getC2() + " " + k.getC3()
						+ ": " + targets.get(k) + "\n");
			}

		}

		buffer.append("uncovered transitions:\n");

		for (Triple<String, String, String> k : targets.keySet()) {

			if (!targets.get(k)) {
				buffer.append(k.getC1() + " " + k.getC2() + " " + k.getC3()
						+ ": " + targets.get(k) + "\n");
			}

		}

		double ratio = Double.NaN;

		if (targets.size() > 0) {
			ratio = 1.0 * covered / targets.size();
		}

		buffer.append("coverage: " + covered + "/" + targets.size() + " "
				+ ratio + "\n");

		buffer.append("execution time (secs):" + ((etime - stime) / 1000.0)
				+ "\n");

	}

	public void setPerformAdaptationStep(boolean b) {

		adaptationStepToBePerformed = b;

	}

	public boolean isAdaptationStepToBePerformed() {
		return adaptationStepToBePerformed;
	}

}
