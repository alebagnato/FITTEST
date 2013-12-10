package eu.fittest.agent.sut.services.serialization.impl;



import eu.fittest.agent.sut.services.serialization.spec.ISerializationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.ObjectFactory;
import eu.fittest.common.core.xml.StartSerialization;
import eu.fittest.common.core.xml.StartSerializationResponse;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class StartSerializationMH extends AbstractMessageHandler<ISerializationService> {

	protected StartSerializationMH(ISerializationService service) {
		super(service);
	}

	public synchronized void onReception(Connection connection, StartSerialization message)	throws FITTESTException {
		String result = getService().startSerialization(message.getResource());
		ObjectFactory f = new ObjectFactory();
		StartSerializationResponse response = f.createStartSerializationResponse();
		response.setAddress(result);
		response.setFrom(message.getTo());
		response.setTo(message.getFrom());
		connection.sendMessage(response);
	}

}
