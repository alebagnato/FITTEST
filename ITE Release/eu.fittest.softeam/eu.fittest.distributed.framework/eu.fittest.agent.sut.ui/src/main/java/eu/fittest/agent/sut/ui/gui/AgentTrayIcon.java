package eu.fittest.agent.sut.ui.gui;

import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Level;
//import eu.fittest.common.util.FittestLogger;

import javax.swing.ImageIcon;

import eu.fittest.agent.sut.services.component.spec.ComponentRegistrationEvent;
import eu.fittest.agent.sut.services.ite.spec.ITERegistrationEvent;
import eu.fittest.agent.sut.services.serialization.spec.SerializationEvent;
import eu.fittest.common.core.service.IServiceListener;
import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.services.forward.spec.ForwardServiceEvent;
import eu.fittest.common.util.ITELogger;

public class AgentTrayIcon extends TrayIcon implements IServiceListener{
	
	private MenuItem _exit;
	private MenuItem _register;
	private MenuItem _enableMessage;
	private boolean _messageEnabled = true;
	
	public void setMessageEnabled(boolean enabled){
		_messageEnabled = enabled;
		setMessageMenuLabel();
	}
	
	public void addExitListener(ActionListener l){
		_exit.addActionListener(l);
	}
	
	public void addRegisterListener(ActionListener l){
		_register.addActionListener(l);
	}
	
	public AgentTrayIcon() {
		super(new ImageIcon(AgentTrayIcon.class.getResource("/icons/fittest.jpg")).getImage());
		PopupMenu popup = new PopupMenu("FITTEST");
	
		_register = new MenuItem("Register");
		_register.setActionCommand("register");
		popup.add(_register);
		
		_enableMessage = new MenuItem();
		_enableMessage.setActionCommand("message");
		popup.add(_enableMessage);
		_enableMessage.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent arg0) {
				_messageEnabled = ! _messageEnabled;
				setMessageMenuLabel();
			}
		});
		
		popup.addSeparator();
		
		_exit = new MenuItem("Exit");
		_exit.setActionCommand("exit");
		popup.add(_exit);	
		
		setMessageMenuLabel();
		setPopupMenu(popup);
	}

	private void setMessageMenuLabel(){
		if(_messageEnabled){
			_enableMessage.setLabel("Disable systray messages");
		}
		else{
			_enableMessage.setLabel("Enable systray messages");
		}
	}
	
	public synchronized void incomingEvent(ServiceEvent event) {
//		FittestLogger.log(Level.FINEST, "new event in Systray: "+event.getClass().getName());
		ITELogger.log(Level.FINEST, "new event in Systray: "+event.getClass().getName());
		if(event instanceof ComponentRegistrationEvent){
			ComponentRegistrationEvent registrationEvent = (ComponentRegistrationEvent)event;
			switch(registrationEvent.getKind()){
				case registration:
					displayMessage(null, "Registration of "+registrationEvent.getData().getFittestComponentId(), MessageType.INFO);
					break;
				case deregistration:
					displayMessage(null, "Deregistration of "+registrationEvent.getData().getFittestComponentId(), MessageType.INFO);
					break;
			}
		}
		else if(event instanceof ITERegistrationEvent){
			ITERegistrationEvent registrationEvent = (ITERegistrationEvent) event;
			switch(registrationEvent.getKind()){
			case Registered:
				displayMessage(null, "Registered as "+registrationEvent.getData().getEntity().getId(), MessageType.INFO);
				break;
			case Unregistered:
				displayMessage(null, "Connection with ITE lost", MessageType.WARNING);
				break;
			default:
				break;
			}
			_register.setEnabled(!registrationEvent.getSource().isRegistered());
		}
		else if(event instanceof SerializationEvent){
			SerializationEvent serializationEvent = (SerializationEvent) event;
			switch(serializationEvent.getKind()){
			case start:
				displayMessage(null, "Start serialization into "+serializationEvent.getResource(), MessageType.INFO);
				break;
			case stop:
				displayMessage(null, "Stop serialization into "+serializationEvent.getResource(), MessageType.INFO);
				break;
			}
		}
		else if(event instanceof ForwardServiceEvent){
			ForwardServiceEvent forwardEvent = (ForwardServiceEvent) event;
			displayMessage(null, forwardEvent.getMessage().getClass().getSimpleName()+" "+forwardEvent.getMessage().getTo(), MessageType.INFO);
		}
	}
	
	@Override
	public void displayMessage(String caption, String text,	MessageType messageType) {
		if(_messageEnabled){
			super.displayMessage(caption, text, messageType);
		}
		else{
			ITELogger.log(Level.INFO, text);
		}
	}

}
