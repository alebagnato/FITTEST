package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.File;
import java.io.IOException;
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
public class AllInOneGenerationHandler extends Efsm2CtAbstractHandler {

	// protected static Logger logger = Logger.getAnonymousLogger();


	/**
	 * the command has been executed, so extract extract the needed information
	 * from the application context.
	 * 
	 * @throws ExecutionException
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		Logs2EFSMHandler h1 = new Logs2EFSMHandler();
		EFSM2MonitorHandler h2 = new EFSM2MonitorHandler();
		
		h1.execute(event);		
		h2.execute(event);
		
		return null;
	}

}
