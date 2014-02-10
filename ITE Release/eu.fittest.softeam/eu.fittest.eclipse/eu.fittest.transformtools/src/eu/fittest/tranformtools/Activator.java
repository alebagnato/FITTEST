package eu.fittest.tranformtools;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "eu.fittest.transformtools"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;
	
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
		_listener = new FolderSelectionListener();
		_listener.hookOnViewer(IFITTESTFolderConstants.NAVIGATOR);
	}
	
	private FolderSelectionListener _listener = null;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
		if(_listener!=null){
			_listener.dispose();
			_listener = null;
		}
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
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path
	 *
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
	
	public static IPath getPluginPath(){
		// first attempt
		URL pluginPath = Platform.getBundle(PLUGIN_ID).getEntry("/");
		try {
			if (pluginPath!= null){
				pluginPath = Platform.resolve(pluginPath);
				return new Path(pluginPath.getPath());
			}
		} catch (IOException e) {
			//e.printStackTrace();
		}
		
		URL url = Platform.find(Platform.getBundle(PLUGIN_ID), new Path("/"));
		if (url!=null) {
			return new Path(url.getPath());
		}
		
		return new Path("/");
	}
}
