package eu.fittest.fbk.efsm2ct.plugin;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Vector;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.service.log.LogService;
import org.osgi.util.tracker.ServiceTracker;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "eu.fittest.fbk.efsm2ct.plugin"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;

	// Variable used in the project's classpath TODO the string is needed also
	// by other plugins
	// TODO it should be defined once somewhere
	private static final String FITTEST_USER_LIBS = "FITTEST_libs";

	private LogService logservice;

	/**
	 * The constructor
	 */
	public Activator() {

	}

	static private IClasspathEntry[] getLibs() {
		Vector<IClasspathEntry> libs = new Vector<IClasspathEntry>();
		Bundle bundle = Platform.getBundle(PLUGIN_ID);
		File libFolder;
		try {
			URL libUrl = bundle.getEntry("lib");

			libFolder = new File(FileLocator.toFileURL(libUrl).getFile());

			for (File f : libFolder.listFiles()) {
				IClasspathEntry entry = JavaCore.newLibraryEntry(
						new Path(f.getAbsolutePath()), null, null);
				libs.add(entry);
			}
		} catch (IOException e) {
			getDefault().osgiLog(LogService.LOG_ERROR, "can't setup library", e);
		}

		return libs.toArray(new IClasspathEntry[libs.size()]);
	}

	static private void createUserLibrary() {
		ClasspathContainerInitializer initializer = JavaCore
				.getClasspathContainerInitializer(JavaCore.USER_LIBRARY_CONTAINER_ID);
		IPath containerPath = new Path(JavaCore.USER_LIBRARY_CONTAINER_ID);
		try {
			initializer.requestClasspathContainerUpdate(
					containerPath.append(FITTEST_USER_LIBS), null,
					new IClasspathContainer() {

						@Override
						public IPath getPath() {
							return new Path(JavaCore.USER_LIBRARY_CONTAINER_ID)
									.append(FITTEST_USER_LIBS);
						}

						@Override
						public int getKind() {
							return K_APPLICATION;
						}

						@Override
						public String getDescription() {
							return FITTEST_USER_LIBS;
						}

						@Override
						public IClasspathEntry[] getClasspathEntries() {
							return getLibs();
						}
					});
		} catch (CoreException e) {
			getDefault().osgiLog(LogService.LOG_ERROR, "can't create user library", e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext context) throws Exception {

		super.start(context);

		System.out.println("##### plugin started.");

		ServiceTracker logServiceTracker = new ServiceTracker(context,
				org.osgi.service.log.LogService.class.getName(), null);
		logServiceTracker.open();
		logservice = (LogService) logServiceTracker.getService();

		if (logservice != null) {
			System.out.println("efsm2ct plugin gets osgi logging service");
			logservice.log(LogService.LOG_ERROR, "efsm2ct plugin started - ver 140203_0922");
		} else {
			System.err.println("efsm2ct plugin can't obtain a the osgi logger.");
		}

		plugin = this;

		createUserLibrary();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in
	 * relative path
	 * 
	 * @param path
	 *            the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	public static IPath getPluginPath() {
		// first attempt
		URL pluginPath = Platform.getBundle(PLUGIN_ID).getEntry("/");
		try {
			if (pluginPath != null) {
				pluginPath = Platform.resolve(pluginPath);
				return new Path(pluginPath.getPath());
			}
		} catch (IOException e) {
			getDefault().osgiLog(LogService.LOG_ERROR, "can't get plugin path", e);
		}

		URL url = Platform.find(Platform.getBundle(PLUGIN_ID), new Path("/"));
		if (url != null) {
			return new Path(url.getPath());
		}

		return new Path("/");
	}

	public void osgiLog(int level, String message) {

		if (logservice != null) {

			logservice.log(level, message);

		} else {
			System.err.println(LogService.LOG_ERROR + " " + message);
		}

	}

	public void osgiLog(int level, String message, Throwable th) {

		if (logservice != null) {

			logservice.log(level, message, th);

		} else {
			System.err.println(LogService.LOG_ERROR + " " + message + " " + th);
			th.printStackTrace(System.err);
		}

	}

}
