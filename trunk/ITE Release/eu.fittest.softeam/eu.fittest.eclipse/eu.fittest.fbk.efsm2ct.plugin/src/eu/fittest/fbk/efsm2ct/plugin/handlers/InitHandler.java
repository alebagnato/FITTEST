package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.osgi.service.log.LogService;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.Configuration;
import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;
import eu.fittest.fbk.efsm2ct.plugin.tool.EFSM2DotStep;
import eu.fittest.fbk.efsm2ct.plugin.tool.EFSM2MonitorStep;
import eu.fittest.fbk.efsm2ct.plugin.tool.Logs2EFSMStep;
import eu.fittest.fbk.efsm2ct.plugin.tool.ToolStepException;
import eu.fittest.fbk.efsm2ct.plugin.utils.GuiHelper;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.plugin.wizards.log2efsm.Logs2EFSMGenerationWizard;
import eu.fittest.fbk.efsm2ct.tools.comp.JavaCompileService;

/**
 * Our sample handler extends AbstractHandler, an IHandler base class.
 * 
 * @see org.eclipse.core.commands.IHandler
 * @see org.eclipse.core.commands.AbstractHandler
 */
public class InitHandler extends Efsm2CtAbstractHandler {

	private static final String EFSM_PROPERTIES_FILE = "efsm.properties";

	// protected static Logger logger = Logger.getAnonymousLogger();

	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 * 
	 * @throws ExecutionException
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		ISelection mySelection = HandlerUtil.getCurrentSelection(event);

		if (!mySelection.isEmpty()) {

			StructuredSelection selection = (StructuredSelection) mySelection;
			IFolder inputFolder = (IFolder) selection.getFirstElement();

			List<IResource> found;

			try {

				found = ResourceUtils.matchRecursively(inputFolder, "log.*\\.xml");

			} catch (CoreException e1) {
				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction: no conf file found in" + inputFolder);
				throw new ExecutionException("No configuration file found");
			}

			if (found.size() == 0) {

				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction: no conf file found in" + inputFolder);
				throw new ExecutionException("No configuration file found");

			}

			IFile file = inputFolder.getFile(EFSM_PROPERTIES_FILE);

			if (!file.exists()) {

				String s = "# filter section\n" + "# filter.regex.1=^HID_.*/.*$\n" + "# filter.regex.2=^IDY_filterPanel_comboboxSeries/close$\n" + "# filter.regex.3=^IDY_filterPanel_comboboxSeries/open$\n";
				s += "efsm2ct.generate.driver=false\n";
				
				InputStream is = new ByteArrayInputStream(s.getBytes());

				try {

					file.create(is, false, null);

				} catch (CoreException ex) {

					throw new ExecutionException("can't initialize folder", ex);

				}
			}

		}

		return null;
	}

}
