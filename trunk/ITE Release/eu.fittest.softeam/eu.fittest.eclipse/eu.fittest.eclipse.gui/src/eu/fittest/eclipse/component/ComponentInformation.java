package eu.fittest.eclipse.component;


public class ComponentInformation {
	private String _name;
	private String _pathInPlugin;
	private String _pluginID;

	public ComponentInformation(String name, String pathInPlugin, String pluginID) {
		_name = name;
		_pathInPlugin = pathInPlugin;
		_pluginID = pluginID;
	}
	
	public String getPluginID() {
		return _pluginID;
	}

	public String getName() {
		return _name;
	}

	public String getPathInPlugin() {
		return _pathInPlugin;
	}
}
