package eu.fittest.common.services.forward.spec;

import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.Message;

public class ForwardServiceEvent extends ServiceEvent<ILocalForwardService>{
	private Message _message;
	
	public ForwardServiceEvent(ILocalForwardService source, Message message) {
		super(source);
		_message = message;
	}

	public Message getMessage(){
		return _message;
	}
}
