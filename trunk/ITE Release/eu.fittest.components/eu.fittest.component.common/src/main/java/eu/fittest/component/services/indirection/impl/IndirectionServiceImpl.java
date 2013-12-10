package eu.fittest.component.services.indirection.impl;

import java.util.ArrayList;
import java.util.List;

import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.component.services.indirection.spec.IIndirectionService;
import eu.fittest.component.services.indirection.spec.IIndirectionServiceListener;

public class IndirectionServiceImpl extends AbstractService implements	IIndirectionService {
	private List<IIndirectionServiceListener> _indirectionListeners;
	
	public IndirectionServiceImpl() {
		_indirectionListeners = new ArrayList<IIndirectionServiceListener>();
		_handlers.add(new InitializeMH(this));
		_handlers.add(new StartMH(this));
		_handlers.add(new StopMH(this));
		_handlers.add(new TerminateMH(this));
		_handlers.add(new RegisterResponseMH(this));
		_handlers.add(new ResourceAvailableMH(this));
	}
	
	@Override
	public String getName() {
		return IIndirectionService.class.getName();
	}

	@Override
	public void addIndirectionServiceListener(IIndirectionServiceListener l) {
		_indirectionListeners.add(l);
	}

	@Override
	public void removeIndirectionServiceListener(IIndirectionServiceListener l) {
		_indirectionListeners.remove(l);
	}

	@Override
	public void initialize(List<Parameter> parameters) {
		for(IIndirectionServiceListener l:_indirectionListeners){
			l.initialize(parameters);
		}
	}

	@Override
	public void start() {
		for(IIndirectionServiceListener l:_indirectionListeners){
			l.start();
		}
	}

	@Override
	public void stop() {
		for(IIndirectionServiceListener l:_indirectionListeners){
			l.stop();
		}
	}

	@Override
	public void terminate() {
		for(IIndirectionServiceListener l:_indirectionListeners){
			l.terminate();
		}
	}

	@Override
	public void registered() {
		for(IIndirectionServiceListener l:_indirectionListeners){
			l.registered();
		}
	}

	@Override
	public void resourceAvailable(String comingFrom, String resource) {
		for(IIndirectionServiceListener l:_indirectionListeners){
			l.resourceAvailable(comingFrom, resource);
		}
	}

}
