package eu.fittest.agent.ite.services.registration.impl;

import eu.fittest.agent.ite.services.registration.spec.IRegistrationService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.RegisterAgent;
import eu.fittest.common.core.xml.RegisterAgentReponse;
import eu.fittest.common.services.message.AbstractMessageHandler;
import eu.fittest.common.services.registration.spec.RegistrationData;

public class RegisterAgentMH extends
		AbstractMessageHandler<IRegistrationService> {

	public RegisterAgentMH(IRegistrationService service) {
		super(service);
	}

	public synchronized void onReception(Connection connection, RegisterAgent message)
			throws FITTESTException {
		if (message.getType() != null) { // if null => possibility of a renew lease after the agent deregistered (or any other possible cause)		
			RegistrationData data = getService().registerAgent(
					connection.getRemoteAddress().split(":")[0],
					new Integer(connection.getRemoteAddress().split(":")[1]),
					message.getEnvironment(), message.getType(),
					message.getDescription(), message.getFrom());
			message.setFrom(data.getEntity().getId());
			message.setTo(data.getIteid());
			RegisterAgentReponse response = FITTESTSingleton.getObjectFactory()
					.createRegisterAgentReponse();
			response.setFittestAgentId(data.getEntity().getId());
			response.setFittestIteId(data.getIteid());
			response.setFittestLease(data.getLease());
			reply(connection, message, response);
		}
	}

	@Override
	public void setIdentityService(IIdentityService identity) {
		// don't consider identity
	}

}
