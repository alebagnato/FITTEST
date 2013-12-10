package eu.fittest.eclipse.gui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.PlatformUI;

import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.gui.wizards.recordingsession.FITTESTRecordingSessionWizard;
import eu.fittest.project.config.TestProject;

public class NewRecordingSessionAction implements IActionDelegate{
	private IStructuredSelection _selection;
	
	public NewRecordingSessionAction() {

	}

	@Override
	public void run(IAction action) {
//		TestProject projectConfig = Activator.getDefault().getActiveProjectConfig();
//		System.out.println(projectConfig.getGeneral().getBaseURL());
		
		FITTESTRecordingSessionWizard wizard = new FITTESTRecordingSessionWizard();
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
