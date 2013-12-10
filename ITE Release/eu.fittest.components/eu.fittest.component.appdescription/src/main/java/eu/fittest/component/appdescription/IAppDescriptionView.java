package eu.fittest.component.appdescription;

import java.beans.PropertyChangeListener;

public interface IAppDescriptionView {
	public void addPropertyChangeListener(PropertyChangeListener l);
	public void removePropertyChangeListener(PropertyChangeListener l);
	void close();
	void open();
}
