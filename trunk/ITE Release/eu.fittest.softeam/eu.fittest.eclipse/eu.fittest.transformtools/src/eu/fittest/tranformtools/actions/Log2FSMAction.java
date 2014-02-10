package eu.fittest.tranformtools.actions;

import java.io.File;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

import eu.fittest.modelInference.fsmInference.config.Config_Launcher;
import eu.fittest.modelInference.fsmInference.manager.FSMmanager;
import eu.fittest.tranformtools.utils.LogFileUtils;
import eu.fittest.tranformtools.wizards.FSMGenWizard;

public class Log2FSMAction implements IWorkbenchWindowActionDelegate {

	Shell shell;

	public Log2FSMAction() {

	}

	public Log2FSMAction(Shell shell) {
		this.shell = shell;
	}

	public void run(IAction action, IFolder input, final IFolder output) {
		FSMGenWizard wizard = new FSMGenWizard(input, output);
		WizardDialog dialog = new WizardDialog(shell, wizard);

		if (dialog.open() == WizardDialog.OK) {
			final String inputFolder = wizard.getInput();
			final String outputFSM = wizard.getOutput();
			final boolean genDotFile = wizard.isGenDotFile();

			final FSMmanager fsmmanager = new FSMmanager();
			final Config_Launcher cl = Config_Launcher.getInstace();

			Job infererringJob = new Job("Inferring FSM model from logs") {

				protected IStatus run(IProgressMonitor monitor) {
					String activeInputFolder = inputFolder;
					if (LogFileUtils.isXMLFormat(inputFolder)) {
						IPath tmpFolder = LogFileUtils.prepareTempFolder();
						activeInputFolder = LogFileUtils.convert2FBKFormat(
								inputFolder, tmpFolder);
						if (activeInputFolder == null) {
							return new Status(IStatus.ERROR,
									"FITTEST tranformation plugin",
									IStatus.ERROR,
									"Problem with log conversion!", null);
						}
					}

					File[] fileList = fsmmanager.getFilelist(activeInputFolder,
							cl.maxFilePemutations);
					/*
					File[] fileCurrent = new File[1];

					monitor.beginTask("Inferring FSM model", fileList.length);

					for (int logIndex = 0; logIndex < fileList.length; logIndex++) {
						fileCurrent[0] = fileList[logIndex];

						monitor.subTask(fileCurrent[0].getName());
						monitor.worked(1);

						if (logIndex == 0)
							fsmmanager.generateFSM(fileCurrent, true, 0);
						else
							fsmmanager.generateFSM(fileCurrent, false, 0);
					}
					*/
					monitor.beginTask("Inferring FSM model", 1);
					fsmmanager.generateFSM(fileList, false, 0);
					monitor.done();
					
					monitor.beginTask("Saving FSM model", 1);
					fsmmanager.FSM2FsmXpr(outputFSM, null, null);

					if (genDotFile) {
						String dotFileName = outputFSM.replace(".fsm", ".dot");
						fsmmanager.FSM2DOT(dotFileName);
					}
					monitor.done();

					shell.getDisplay().asyncExec(new Runnable() {

						public void run() {
							MessageDialog
									.openInformation(shell, "Model inference",
											"Model inference completed, check the specified output file for result !");
						}
					});
					try {
						output.refreshLocal(IResource.DEPTH_INFINITE, null);
					} catch (CoreException e) {
						e.printStackTrace();
					}
					return new Status(IStatus.OK,
							"FITTEST tranformation plugin", IStatus.OK,
							"Finish inferring model from logs!", null);
				}

			};

			infererringJob.schedule();

		}
	}

	public void run(IAction action) {
		run(action, null, null);
	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(IWorkbenchWindow window) {
		this.shell = window.getShell();
	}

}
