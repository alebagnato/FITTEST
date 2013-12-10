package eu.fittest.eclipse.component.junit;

import java.io.File;
import java.io.IOException;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class Activator extends AbstractUIPlugin {
	public static final String PLUGIN_ID = "eu.fittest.eclipse.component.junit"; //$NON-NLS-1$
	
	
	// The shared instance
	private static Activator plugin;
	private static final String FITTEST_USER_LIBS = "FITTEST_libs";
	
	/**
	 * The constructor
	 */
	public Activator() {
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		createUserLibrary();
	}

	static private IClasspathEntry[] getLibs(){
		Vector<IClasspathEntry> libs = new Vector<IClasspathEntry>(); 
		Bundle bundle = Platform.getBundle(PLUGIN_ID);		
		File libFolder;
		try {
			libFolder = new File(FileLocator.toFileURL(bundle.getEntry("resources/lib")).getFile());
			for(File f: libFolder.listFiles()){
				IClasspathEntry entry = JavaCore.newLibraryEntry(new Path(f.getAbsolutePath()), null, null);
				libs.add(entry);
			}
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}	
		
		return libs.toArray(new IClasspathEntry[libs.size()]);
	}
	
	static private void createUserLibrary(){
		ClasspathContainerInitializer initializer= JavaCore.getClasspathContainerInitializer(JavaCore.USER_LIBRARY_CONTAINER_ID);
		IPath containerPath = new Path(JavaCore.USER_LIBRARY_CONTAINER_ID);
		try {
			initializer.requestClasspathContainerUpdate(containerPath.append(FITTEST_USER_LIBS), null, new IClasspathContainer() {
				
				@Override
				public IPath getPath() {
					return new Path(JavaCore.USER_LIBRARY_CONTAINER_ID).append(FITTEST_USER_LIBS) ;
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
			e.printStackTrace();
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
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
}
