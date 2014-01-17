package eu.fittest.eclipse.model.jobs;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import org.eclipse.core.resources.IFile;

public class TestCase {
	private IFile _file;
	private boolean _selected;
	
	private PropertyChangeSupport _support;
	
	public void addPropertyChangeListener(String name, PropertyChangeListener l){
		_support.addPropertyChangeListener(name, l);
	}

	public void removePropertyChangeListener(String name, PropertyChangeListener l){
		_support.removePropertyChangeListener(name, l);
	}
	
	public TestCase(IFile file) {
		_support = new PropertyChangeSupport(this);
		_file = file;
		_selected = false;
	}
	
	public IFile getFile() {
		return _file;
	}

	public boolean isSelected() {
		return _selected;
	}
	
	public void setSelected(boolean selected){
		_support.firePropertyChange("selected", _selected, _selected = selected);
	}
}
