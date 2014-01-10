/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.tranformtools.popup.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import eu.fittest.eventBasedFSM.EventBasedFSM;
import eu.fittest.tranformtools.utils.LogFileUtils;

public class TraceToFSMAction extends AbstractHandler implements IObjectActionDelegate {

	private Shell _shell;
	private IFolder[] _selectedFolders; 
	
	public void run(IAction action) {
		if (_selectedFolders != null && _selectedFolders.length > 0) {
			IFolder folder = _selectedFolders[0]; // first file only
			
			try {
				List<IFile> xmlFiles = LogFileUtils.collectFiles(folder, "xml", "log_");
				List<IFile> trcFiles = LogFileUtils.collectFiles(folder, "trc", "log_");
				
				String ext = "txt";
				if (xmlFiles.size() > 0 && trcFiles.size() == 0)
					ext = "xml";
				else if (xmlFiles.size() == 0 && trcFiles.size() > 0)
					ext = "trc";
				else if (xmlFiles.size() > trcFiles.size())
					ext = "xml";
				else 
					ext = "trc";
					
				
				EventBasedFSM.computeFSMfromTraces(folder.getLocation().toOSString(),
						new File(folder.getLocation().toFile(), folder.getName()+".fsm").getAbsolutePath(), ext);
				folder.refreshLocal(IFolder.DEPTH_INFINITE, null);
				
				MessageDialog.openInformation(_shell, "FSM generation",
						"Model inferred successully, please check the output folder!");
			} catch (final Exception e) {
				_shell.getDisplay().asyncExec(new Runnable() {
					public void run() {
						MessageDialog.openError(_shell,
										"FSM generation",
										"Something went wrong, please make sure the log files are welformed!\n"
										+ "You can also check out the eclipse log to have more info about the issue.");
					}
				});
			}
			
		}

	}


	public void selectionChanged(IAction action, ISelection selection) {
		ArrayList<IFolder> sFolders = new ArrayList<IFolder>();
		IFolder folder = null;
		if (selection != null && selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;

			for (Iterator iter = ss.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof IFolder) {
					folder = (IFolder)obj;
				} else if (obj instanceof IAdaptable) {
					IAdaptable a = (IAdaptable) obj;
					IResource res = (IResource) a.getAdapter(IResource.class);
					if (res instanceof IFolder) {
						folder = (IFolder) res;
					}
				}

				if (folder != null && folder.isSynchronized(IResource.DEPTH_ZERO)) {
					sFolders.add(folder);
				} else if (!folder.isSynchronized(IResource.DEPTH_ZERO)) {
					MessageDialog.openInformation(_shell, "Synchronization error",
					"The selected folder is not synchronized with the system file, please refresh the project!");
				}
			}
		}

		if (sFolders.isEmpty()) {
			_selectedFolders = null;
		} else {
			_selectedFolders = sFolders.toArray(new IFolder[sFolders.size()]);
		}

	}

	public void setActivePart(IAction action, IWorkbenchPart targetPart){
		_shell = targetPart.getSite().getShell();
	}


	public Object execute(ExecutionEvent event) throws ExecutionException {
		_shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		run(null);
		return null;
	}

}
