package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.framework.Bundle;
import org.osgi.service.log.LogService;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.plugin.Efsm2CtException;
import eu.fittest.fbk.efsm2ct.plugin.tool.AdaptationStep;
import eu.fittest.fbk.efsm2ct.plugin.utils.GuiHelper;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.tools.evosuite.LogConsumer;
import eu.fittest.fbk.efsm2ct.tools.evosuite.ProcessSpawnException;
import eu.fittest.fbk.efsm2ct.tools.files.FileFinder;
import eu.fittest.fbk.efsm2ct.tools.txl.TxlService;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * 
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class AdaptEvosuiteTestsHandler extends Efsm2CtAbstractHandler {

	public enum TargetType {
		SELENIUM_JUNIT, FLEXDRIVE_TESTS
	};

	private TargetType targetType = TargetType.SELENIUM_JUNIT;

	// private static Logger logger = Logger.getAnonymousLogger();

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 */
	public Object execute(ExecutionEvent event) throws ExecutionException {

		ISelection mySelection = HandlerUtil.getCurrentSelection(event);
		final Shell myShell = HandlerUtil.getActiveShell(event);

		// logger.info("run:" + event);

		if (!mySelection.isEmpty()) {

			StructuredSelection selection = (StructuredSelection) mySelection;

			IFolder sessionFolder = (IFolder) selection.getFirstElement();

			IFolder outputFolder = (IFolder) sessionFolder.findMember("output");

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

			String sessionName = sessionFolder.getName();

			IProject project = sessionFolder.getProject();

			IFolder evoTestsFolder = outputFolder.getFolder("evosuite-tests");

			IFolder testSuitesDirectory = project
					.getFolder(IFITTESTFolderConstants.TEST_SUITES);

			String txlTransformation = getTxtTransformation();

			String additionalMethodFile = getAdditionalMethodFile();

			try {

				File txlTransformationDir = new File(
						lookForEvosuiteBundlePath("resources"), "txl");

				int status = AdaptationStep.performAdaptation(
						txlTransformationDir, sessionName, evoTestsFolder,
						testSuitesDirectory, txlTransformation,
						additionalMethodFile);

				if (status == 0) {

					refresh(project);

				} else {
					throw new Efsm2CtException("txl exits with error status:"
							+ status);

				}
			} catch (Exception e) {
				Activator.getDefault().osgiLog(LogService.LOG_ERROR,
						"Error transforming tests", e);

				GuiHelper.showError(myShell, "Error in transforming tests", e);
			}

			//
		}
		return null;

	}

	private String getAdditionalMethodFile() {

		switch (targetType) {

		case FLEXDRIVE_TESTS:

			return "additional_methods.java";

		case SELENIUM_JUNIT:
			return "additional_methods_selenium.java";

		}

		// should never reach this point

		return null;
	}

	private String getTxtTransformation() {

		switch (targetType) {

		case FLEXDRIVE_TESTS:

			return "transform_evotest_to_sut.txl";

		case SELENIUM_JUNIT:
			return "transform_evotest_to_selenium.txl";

		}

		// should never reach this point

		return null;

	}

}
