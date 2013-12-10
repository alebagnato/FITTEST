package eu.fittest.common.core.service;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractLocalService extends AbstractRemoteService
		implements ILocalService {

	private List<IServiceListener> _listeners;

	protected IServiceRegistry _registry;

	private List<IServiceListener> _listenersToDelete;
	private List<IServiceListener> _listenersToAdd;
	private boolean _ongoing = false;

	protected AbstractLocalService() {
		_listeners = new ArrayList<IServiceListener>();
		_listenersToDelete = new ArrayList<IServiceListener>();
		_listenersToAdd = new ArrayList<IServiceListener>();
	}

	public synchronized void addServiceListener(IServiceListener listener) {
		if (_ongoing) {
			if (!_listenersToAdd.contains(listener))
				_listenersToAdd.add(listener);
		} else {
			if (!_listeners.contains(listener))
				_listeners.add(listener);
		}
	}

	public synchronized void removeServiceListener(IServiceListener listener) {
		if (_ongoing) {
			if (!_listenersToDelete.contains(listener))
				_listenersToDelete.add(listener);
		} else {
			_listeners.remove(listener);
		}
	}

	protected synchronized void fireEvent(ServiceEvent event) {
		_ongoing = true;
		for (IServiceListener l : _listeners) {
			l.incomingEvent(event);
		}
		_reentrantRemoving();
		_reentrantAdding();
		_ongoing = false;
	}

	private void _reentrantRemoving() {
		for (IServiceListener l : _listenersToDelete) {
			_listeners.remove(l);
		}
		_listenersToDelete.clear();
	}

	private void _reentrantAdding() {
		for (IServiceListener l : _listenersToAdd) {
			if (!_listeners.contains(l))
				_listeners.add(l);
		}
		_listenersToAdd.clear();
	}

}
