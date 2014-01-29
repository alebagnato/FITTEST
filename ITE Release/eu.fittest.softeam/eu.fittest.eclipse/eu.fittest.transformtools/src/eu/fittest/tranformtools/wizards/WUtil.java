package eu.fittest.tranformtools.wizards;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;

public class WUtil {
	
	public static void createSpace(Composite composite){
		Label label = new Label(composite, SWT.NONE);
		GridData dg = new GridData();
		dg.horizontalSpan = 3;
		dg.heightHint = 3;
		label.setLayoutData(dg);
	}
	
	/**
	 * Ask user to select an element from the workspace
	 * @param title
	 * @param msg
	 * @return
	 */
	public static String getSelection(Shell shell, String title, String msg){
		
		ILabelProvider lp = new WorkbenchLabelProvider();
		ITreeContentProvider cp = new BaseWorkbenchContentProvider(); 
		
		ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(shell,lp, cp);
		dialog.setValidator(null);
		dialog.setAllowMultiple(false);
		dialog.setTitle(title); 
		dialog.setMessage(msg);
		dialog.setInput(ResourcesPlugin.getWorkspace().getRoot());
		
		if (dialog.open() == ElementTreeSelectionDialog.OK) {
			Object[] elements = dialog.getResult();
			if (elements.length == 1) {
				return ((IResource)elements[0]).getLocation().toOSString();
			}
		} 
		
		return "";
	}

}
