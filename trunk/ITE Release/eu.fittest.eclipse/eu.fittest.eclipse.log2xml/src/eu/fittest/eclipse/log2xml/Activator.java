package eu.fittest.eclipse.log2xml;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
//import eu.fittest.eclipse.log2xml.utils.XMLSelectionListener;
import eu.fittest.eclipse.log2xml.actions.XMLSelectionListener;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "eu.fittest.eclipse.log2xml"; 

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
		_listener = new XMLSelectionListener();
		_listener.hookOnViewer(IFITTESTFolderConstants.NAVIGATOR);
	}
	
	private XMLSelectionListener _listener = null;

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

}
