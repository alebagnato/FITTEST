package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.osgi.framework.internal.core.BundleURLConnection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.framework.Bundle;
import org.osgi.service.log.LogService;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.plugin.Efsm2CtException;
import eu.fittest.fbk.efsm2ct.plugin.utils.GuiHelper;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.plugin.utils.TransitionsCoverageChecker;
import eu.fittest.fbk.efsm2ct.plugin.utils.TransitionsTracker;
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

			Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Error running evosuite, can't get a LogView instance, abort execution");
			throw new ExecutionException("can't get a LogView instance, abort execution");

		}

		// logger.info("run:" + event);

		if (!mySelection.isEmpty()) {

			StructuredSelection selection = (StructuredSelection) mySelection;

			org.eclipse.core.internal.resources.Folder fe = (org.eclipse.core.internal.resources.Folder) selection.getFirstElement();

			IProject project = fe.getProject();

			IFolder outputFolder = (IFolder) fe.findMember("output");

			List<IResource> matched;

			try {
				matched = ResourceUtils.match(outputFolder, "^.*\\.efsm$");
			} catch (CoreException e1) {
				throw new ExecutionException("internal error", e1);
			}

			if (matched.size() != 1) {
				throw new ExecutionException("internal error: invalid number of resource selected:" + matched.size());
			}

			File fsmFile = matched.get(0).getLocation().toFile();

			IFolder binFolder = project.getFolder("/bin");

			try {

				String packageNamePrefix = fe.getName();

				String packagePathStr = packageNamePrefix.replace(".", File.separator);

				String sutName = fsmFile.getName().replace(".efsm", "");

				// IFolder libFolder = project.getFolder("/lib");

				File projectDir = project.getLocation().toFile();

				// File libDir = libFolder.getLocation().toFile();
				File binDir = binFolder.getLocation().toFile();

				TransitionsTracker tt = new TransitionsTracker(fsmFile.getAbsolutePath());

				String classQualifiedName = packageNamePrefix + "." + "Test" + sutName;

				scheduleRunEvosuite(packageNamePrefix, classQualifiedName, project, outputFolder.getLocation().toFile(), binDir, tt);

			} catch (Exception e) {

				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Error running evosuite", e);
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

	private void runEvosuite(String packagePrefix, String classQualifiedName, File workingDir, File binDir, TransitionsCoverageChecker checker) throws IOException, ProcessSpawnException, URISyntaxException, ExecutionException {

		boolean okToRun = false;

		String libPath = lookForEvosuiteBundlePath("lib");
		File libDir = new File(libPath);

		EvosuiteService es = new EvosuiteService(classQualifiedName);

		es.setEvosuiteDirPath(libPath);
		es.setWorkingDirectory(workingDir.getAbsolutePath());
		es.setEvosuiteJarName("evosuite-minimal.jar");
		es.addClasspath(binDir.getAbsolutePath());

		String[] libs = new String[] { "eu.fittest.common.jar", "eu.fittest.fbk.efsm2ct.efsm2mon.jar", "eu.fittest.fbk.efsm2ct.flexdrv.jar", "evosuite-minimal.jar", "flashselenium-java-client-extension.jar", "guava.jar", "selenium-api.jar", "selenium-chrome-driver.jar", "selenium-java.jar", "selenium-remote-driver.jar"

		};

		for (String l : libs) {
			es.addClasspath(new File(libDir, l).getAbsolutePath());
		}

		// mandatory property values
		es.addProperty(EvosuiteService.shutdown_method, "_shutdown");
		es.addProperty(EvosuiteService.startup_method, "_startup");

		// GA related
		es.addProperty(EvosuiteService.population, Integer.toString(ConfigurationFactory.getInstance().getEvosuitePopulationSize()));
		es.addProperty(EvosuiteService.chromosome_length, Integer.toString(ConfigurationFactory.getInstance().getEvosuiteChromosomeLength()));

		// search process timings
		es.addProperty(EvosuiteService.timeout, "300000");
		es.addProperty(EvosuiteService.search_budget, "1000000");
		es.addProperty(EvosuiteService.global_timeout, "60000"); // seconds
		es.addProperty(EvosuiteService.minimization_timeout, "800000"); // almost
																		// infinite

		es.addProperty(EvosuiteService.report_dir, "evosuite-reports/" + packagePrefix.replace('.', File.separatorChar));

		// other properties
		es.addProperty(EvosuiteService.stopping_port, Integer.toString(ConfigurationFactory.getInstance().getEvosuiteStoppingPort()));
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

	

	private void scheduleRunEvosuite(final String packagePrefix, final String classQualifiedName, final IProject project, final File workingDir, final File binDir, final TransitionsTracker tt) {

		Job generateJob = new Job("Generate Test Cases") {

			@Override
			protected IStatus run(final IProgressMonitor monitor) {

				int evosuteShutdownPort = ConfigurationFactory.getInstance().getEvosuiteStoppingPort();

				TransitionsCoverageChecker checker = new TransitionsCoverageChecker(tt, evosuteShutdownPort, monitor, logView);

				monitor.beginTask("Generating test cases...", tt.size());

				try {
					// TODO how to show logView?
					// logView.getSite().getWorkbenchWindow().getActivePage().showView("eu.fittest.fbk.efsm2ct.plugin.views.log");

					// showView("eu.fittest.fbk.efsm2ct.plugin.views.log");

					logView.setFocus();
					logView.showBusy(true);
					runEvosuite(packagePrefix, classQualifiedName, workingDir, binDir, checker);
					logView.showBusy(false);
					monitor.done();
					refresh(project);
					logView.setFocus();
					logView.scheduleTextUpdate("Run terminated");
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

}
