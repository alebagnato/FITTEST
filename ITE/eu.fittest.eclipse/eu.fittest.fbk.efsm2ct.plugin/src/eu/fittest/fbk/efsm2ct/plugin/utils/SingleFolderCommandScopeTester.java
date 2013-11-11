package eu.fittest.fbk.efsm2ct.plugin.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;
import org.osgi.service.log.LogService;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.ConfigurationFactory;

public class SingleFolderCommandScopeTester extends PropertyTester {

	public SingleFolderCommandScopeTester() {
	}

	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {

		if (receiver instanceof List) {

			@SuppressWarnings("rawtypes")
			List receiverAsList = (List) receiver;

			if (receiverAsList.size() == 1) {

				IResource resource = (IResource) receiverAsList.get(0);

				// selected item must be a folder

				if (resource instanceof IFolder) {

					IFolder folder = (IFolder) resource;

					if (folder.findMember("efsm.properties") != null) {

						try {
							
							List<IResource> files1 = ResourceUtils
									.matchRecursively(folder, "^.*\\.efsm$");
							
							List<IResource> files2 = ResourceUtils
									.matchRecursively(folder, "^gensrc$");
							
							List<IResource> files3 = ResourceUtils
									.matchRecursively(folder, "^evosuite-tests$");

							if (property.endsWith("efsmgeneration")) {

								return files1.isEmpty();

							} else if (property.endsWith("monitorgeneration")) {

								return !files1.isEmpty() && files2.isEmpty();

							} else if (property.endsWith("evotestsearch")) {

								return !files1.isEmpty() && !files2.isEmpty() && files3.isEmpty();

							} else if (property.endsWith("evotransform")) {
								
								return !files1.isEmpty() && !files2.isEmpty() && !files3.isEmpty();
							}
							
						} catch (Exception ex) {

						}

					} else {
						
						if (property.endsWith("init")) {
							
							try {
								
								List<IResource> files = ResourceUtils.matchRecursively(folder, "log.*\\.xml");
								
								return files.size() > 0;
								
							} catch (CoreException e) {
								
							}
							
						}
						
					}

				}
			}
		}

		return false;

	}

}
