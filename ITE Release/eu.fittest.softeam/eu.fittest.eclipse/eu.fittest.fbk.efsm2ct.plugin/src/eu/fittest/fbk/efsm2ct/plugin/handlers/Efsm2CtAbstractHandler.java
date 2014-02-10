package eu.fittest.fbk.efsm2ct.plugin.handlers;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.framework.internal.core.BundleURLConnection;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Bundle;
import org.osgi.service.log.LogService;

import eu.fittest.fbk.efsm2ct.plugin.Activator;
import eu.fittest.fbk.efsm2ct.plugin.Efsm2CtException;
import eu.fittest.fbk.efsm2ct.plugin.utils.ResourceUtils;
import eu.fittest.fbk.efsm2ct.plugin.views.LogView;

public abstract class Efsm2CtAbstractHandler extends AbstractHandler {
	

	protected static String ensureDirectoryExists(String path, IContainer parent) {

		Path genSrcPath = new Path(path); 
		IFolder genSrcFolder = parent.getFolder(genSrcPath);

		String outputDriverDirectory = genSrcFolder.getLocation().toFile().getAbsolutePath();

		File outputDriverDirectoryPath = new File(outputDriverDirectory);

		if (!outputDriverDirectoryPath.exists()) {
			outputDriverDirectoryPath.mkdirs();
		}

		return outputDriverDirectory;
	}
	
	protected static void refresh(IProject project) throws ExecutionException {
		try {
			project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		} catch (CoreException e) {
		
			throw new ExecutionException("can't refresh the project",e);
			
		}
	}
	
	protected static String recoverPackageNamePrefix(IFolder fe, final String topFolderName) throws Efsm2CtException {

		if (fe.findMember("eu") != null) {

			StringBuilder sb = new StringBuilder();
			IContainer cfolder = fe;

			while (!cfolder.getName().equals(topFolderName)) {

				sb.insert(0, ".");
				sb.insert(0, cfolder.getName());

				cfolder = cfolder.getParent();
			}

			return sb.substring(0, sb.length() - 1);

		} else {
			throw new Efsm2CtException(
					"you have to select a proper package suffix");
		}

	}
	
	protected static LogView lookForLogView() {
		
		LogView logView = null;
		
		IWorkbench wb = PlatformUI.getWorkbench();
		IWorkbenchWindow[] wins = wb.getWorkbenchWindows();

		for (IWorkbenchWindow w : wins) {

			System.out.println("workbench window: " + w.toString());

			IWorkbenchPage[] pages = w.getPages();

			for (IWorkbenchPage p : pages) {

				System.out.println("page label: " + p.getLabel());

				IViewReference[] viewReferences = p.getViewReferences();

				for (IViewReference v : viewReferences) {

					System.out.println("view reference id: " + v.getId());

					if (v.getId().equals(
							"eu.fittest.fbk.efsm2ct.plugin.views.log")) {

						logView = (LogView) v.getView(true);

					}

				}

			}

		}
		
		return logView;
		
	}

	protected static String lookForEvosuiteBundlePath(String entryStr) throws IOException, URISyntaxException, ExecutionException {

		try {

			Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);

			URL entry = bundle.getEntry(entryStr);

			// Activator.getDefault().osgiLog(LogService.LOG_WARNING, "lib's url:" + entry);

			if (entry != null) {

				URLConnection connection = entry.openConnection();

				if (connection instanceof BundleURLConnection) {

					URL fileURL = ((BundleURLConnection) connection).getFileURL();

					URI uri = new URI(fileURL.toString());

					String path = new File(uri).getAbsolutePath();

					return path;

				} else {
					Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Wrong connection instance:" + connection);
					throw new ExecutionException("Wrong connection instance:" + connection);
				}

			} else {
				Activator.getDefault().osgiLog(LogService.LOG_ERROR, "Can't get bundle url");
				throw new ExecutionException("Can't get bundle url");
			}

		} catch (NullPointerException ex) {

			Activator.getDefault().osgiLog(LogService.LOG_ERROR, "empty entry 'lib' in bundle",ex);
			throw ex;

		}

	}
	
	protected static Properties getEfsmProperties(IFolder inputFolder) throws ExecutionException {
		List<IResource> found;
		try {
			found = ResourceUtils.match(inputFolder, "efsm.properties");
		} catch (CoreException e1) {
			Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction: no conf file found in"+inputFolder);
			throw new ExecutionException("No configuration file found");
		}
		
		if (found.size() != 1) {
			
			Activator.getDefault().osgiLog(LogService.LOG_ERROR, "error in EFSM2MonitorAction: no conf file found in"+inputFolder);
			throw new ExecutionException("No configuration file found");
			
		}
		
		IFile propertiesFile = (IFile) found.get(0);
		
		File filterFile = propertiesFile.getLocation().toFile();
		
		Properties prop = new Properties();
		
		try {
		
		prop.load(new FileInputStream(filterFile));
		
		} catch (IOException ex) {
			throw new ExecutionException("can't load configuration file", ex);
		}
		
		return prop;
	}
	
}
