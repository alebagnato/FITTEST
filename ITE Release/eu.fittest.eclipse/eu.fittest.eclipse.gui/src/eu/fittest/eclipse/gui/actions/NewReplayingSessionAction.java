package eu.fittest.eclipse.gui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.PlatformUI;

import eu.fittest.eclipse.gui.wizards.replayingsession.FITTESTReplayingSessionWizard;

public class NewReplayingSessionAction implements IActionDelegate {
	private IStructuredSelection _selection;
	
	public NewReplayingSessionAction() {
	}

	@Override
	public void run(IAction action) {
		FITTESTReplayingSessionWizard wizard = new FITTESTReplayingSessionWizard();
		wizard.init(PlatformUI.getWorkbench(), _selection);
		WizardDialog dialog = new WizardDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
		dialog.open();
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection)
			_selection = (IStructuredSelection)selection;
	}

}
