package eu.fittest.common.core.exception;

import java.util.ArrayList;
import java.util.List;

public class AbstractFITTESTExceptionObservable {
	private List<FITTESTExceptionListener> _listeners;
	
	protected AbstractFITTESTExceptionObservable() {
		_listeners = new ArrayList<FITTESTExceptionListener>();
	}
	
	protected void fireException(Throwable t){
		for(FITTESTExceptionListener l: _listeners){
			l.uncaughtException(t);
		}
	}
	
	public void addFITTESTExceptionListener(FITTESTExceptionListener l){
		_listeners.add(l);
	}
	
	public void removeFITTESTExceptionListener(FITTESTExceptionListener l){
		_listeners.remove(l);
	}
}
