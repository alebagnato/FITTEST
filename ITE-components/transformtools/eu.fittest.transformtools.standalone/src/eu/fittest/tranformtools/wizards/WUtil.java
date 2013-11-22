/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
		ILabelProvider lp= new WorkbenchLabelProvider();
		ITreeContentProvider cp= new BaseWorkbenchContentProvider(); 
		
		ElementTreeSelectionDialog dialog = new ElementTreeSelectionDialog(shell,lp, cp);
		dialog.setValidator(null);
		dialog.setAllowMultiple(false);
		dialog.setTitle(title); 
		dialog.setMessage(msg);
		dialog.setInput(ResourcesPlugin.getWorkspace().getRoot());
		
		if (dialog.open() == ElementTreeSelectionDialog.OK) {
			Object[] elements= dialog.getResult();
			if (elements.length == 1) {
				String temp = ((IResource)elements[0]).getLocation().toOSString();
				return temp;
			}
		} 
		
		return "";
	}

}
