package eu.fittest.tranformtools.popup.actions;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import eu.fittest.tranformtools.wizards.CTEGenWizard;
import eu.fittest.tranformtools.wizards.XInputGenWizard;

public class FSM2XinputAction implements IObjectActionDelegate {

	private IFile[] selectedFile; 
	private Shell shell;
	
	/**
	 * Constructor for Action1.
	 */
	public FSM2XinputAction() {
		super();
	}

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 */
	public void run(IAction action) {
		if (selectedFile != null && selectedFile.length > 0) {
			IFile file = selectedFile[0]; // first file only
			XInputGenWizard generationWizard = new XInputGenWizard(file.getLocation().toOSString());
			WizardDialog dialog = new WizardDialog(shell, generationWizard);
			if (dialog.open() == WizardDialog.OK){
//				MessageDialog.openInformation(shell, "CTE tree generation", "CTE trees are being generated, checking the process view for details!");
			}
		}
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		ArrayList<IFile> newFiles = new ArrayList<IFile>();
		IFile file = null;
		if (selection != null && selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;

			for (Iterator iter = ss.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof IFile) {
					file = (IFile) obj;
				} else if (obj instanceof IAdaptable) {
					IAdaptable a = (IAdaptable) obj;
					IResource res = (IResource) a.getAdapter(IResource.class);
					if (res instanceof IFile) {
						file = (IFile) res;
					}
				}

				if (file != null && file.isSynchronized(IResource.DEPTH_ZERO)) {
					newFiles.add(file);
				} else if (!file.isSynchronized(IResource.DEPTH_ZERO)) {
					MessageDialog.openInformation(shell, "Synchronization error",
					"The selected file is not synchronized with the system file, please refresh the project!");
				}
			}
		}

		if (newFiles.isEmpty()) {
			selectedFile = null;
			// action.setEnabled(false);
		} else {
			selectedFile = (IFile[]) newFiles.toArray(new IFile[newFiles.size()]);
			// action.setEnabled(true);
		}
	}

}
