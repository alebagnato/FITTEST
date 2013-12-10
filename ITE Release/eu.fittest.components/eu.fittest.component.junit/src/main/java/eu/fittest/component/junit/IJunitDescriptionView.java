package eu.fittest.component.junit;

import java.beans.PropertyChangeListener;

public interface IJunitDescriptionView {
	public void addPropertyChangeListener(PropertyChangeListener l);
	public void removePropertyChangeListener(PropertyChangeListener l);
	void close();
	void open();
	WebBrowser getSelectedWebBrowser();
	public void appendText(String text);
}
