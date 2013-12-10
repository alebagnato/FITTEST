package eu.fittest.component.appdescription;

import java.awt.GraphicsEnvironment;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTConnectionClosedException;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTExceptionListener;
import eu.fittest.common.core.xml.Initialize.Parameter;
import eu.fittest.component.appdescription.gui.AppDescriptionView;
import eu.fittest.component.appdescription.headless.AppDescriptionNoUI;
import eu.fittest.component.appdescription.ite.services.properties.impl.PropertiesServiceImpl;
import eu.fittest.component.appdescription.ite.services.properties.spec.IPropertiesService;
import eu.fittest.component.common.AbstractComponent;

public class AppDescriptionComponent extends AbstractComponent implements PropertyChangeListener, FITTESTExceptionListener{	
	private IAppDescriptionView _view;
	private boolean _oneShot = false;
	
	public AppDescriptionComponent(int port)throws FITTESTException {
		super(AppDescriptionComponent.class.getSimpleName(), port);
		_serviceRegistry.registerService(new PropertiesServiceImpl());
	}

	@Override
	public void register() throws FITTESTException {
		super.register();
		_connection.addFITTESTExceptionListener(this);
	}
	
	@Override
	public void registered() {
		if(!_oneShot){
			_oneShot = true;
			try {
				if(GraphicsEnvironment.isHeadless()){
					_view = new AppDescriptionNoUI();
				}
				else{
					_view = new AppDescriptionView();
				}
				
				_view.addPropertyChangeListener(this);
				_view.open();
			} catch (FITTESTException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}		
	}
	
	@Override
	public void uncaughtException(Throwable t) {
		if(t instanceof FITTESTConnectionClosedException){
			Logger.getAnonymousLogger().log(Level.INFO, "Connection to agent closed, exiting view");
			_view.close();
			FITTESTSingleton.shutdown();
		}
	}
	
	@Override
	public void initialize(List<Parameter> parameters) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void start() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void stop() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void terminate() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		String newValue = null;
		if(evt.getNewValue()!=null){
			newValue = evt.getNewValue().toString();
		}
		Logger.getAnonymousLogger().log(Level.INFO, evt.getPropertyName()+": "+newValue);
		_serviceRegistry.findService(IPropertiesService.class).setProperty(evt.getPropertyName(), newValue);		
	}

}
