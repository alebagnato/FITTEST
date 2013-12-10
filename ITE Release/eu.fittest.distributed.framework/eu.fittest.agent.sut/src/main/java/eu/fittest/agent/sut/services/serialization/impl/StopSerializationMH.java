package eu.fittest.agent.sut.services.serialization.impl;



import eu.fittest.agent.sut.services.serialization.spec.ISerializationService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.StopSerialization;
import eu.fittest.common.services.message.AbstractMessageHandler;


public class StopSerializationMH extends AbstractMessageHandler<ISerializationService> {

	protected StopSerializationMH(ISerializationService service) {
		super(service);
	}

	public synchronized void onReception(Connection connection, StopSerialization message) throws FITTESTException {
		getService().stopSerialization(message.getResource());
	}
}
