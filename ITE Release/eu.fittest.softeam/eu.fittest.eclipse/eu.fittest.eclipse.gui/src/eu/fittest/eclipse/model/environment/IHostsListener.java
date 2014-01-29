package eu.fittest.eclipse.model.environment;

// by urueda
public interface IHostsListener {

	public void notifyHostAdded(Host host);

	public void notifyComponentAdded(Host host, String componentId);

	public void notifyHostRemoved(Host host);

	public void notifyComponentRemoved(Host host, String componentId);

}
