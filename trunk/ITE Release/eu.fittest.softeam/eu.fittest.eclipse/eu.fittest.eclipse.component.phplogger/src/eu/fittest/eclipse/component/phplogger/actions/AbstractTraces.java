package eu.fittest.eclipse.component.phplogger.actions;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import eu.fittest.eclipse.component.phplogger.wizard.traces.AbstractTracesWizard;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;

public class AbstractTraces implements IObjectActionDelegate{

	private IFolder _selection;
	private Shell _shell;

	public AbstractTraces() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void run(IAction action) {
		if (_selection != null) {
			IFolder output = _selection.getProject().getFolder(IFITTESTFolderConstants.MODELS);
			AbstractTracesWizard generationWizard = new AbstractTracesWizard(_selection, output);
			WizardDialog dialog = new WizardDialog(_shell, generationWizard);
			if (dialog.open() == WizardDialog.OK){
//				MessageDialog.openInformation(shell, "CTE tree generation", "CTE trees are being generated, checking the process view for details!");
			}
		}
		
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection)
			_selection = (IFolder) ((IStructuredSelection)selection).getFirstElement();		
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		_shell = targetPart.getSite().getShell();
	}

}
