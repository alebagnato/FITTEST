package eu.fittest.common.core.connection.impl;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.CrossDomainPolicy;
import eu.fittest.common.core.xml.PolicyFileRequest;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class PolicyFileMH extends AbstractMessageHandler<IConnectionService>{

	protected PolicyFileMH(IConnectionService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, PolicyFileRequest message) throws FITTESTException {
		CrossDomainPolicy policy = FITTESTSingleton.getObjectFactory().createCrossDomainPolicy();
		policy.setAllowAccessFrom(FITTESTSingleton.getObjectFactory().createCrossDomainPolicyAllowAccessFrom());
		policy.getAllowAccessFrom().setDomain("*");
		policy.getAllowAccessFrom().setToPorts("*");
		connection.sendMessage(policy);
	}
	
	@Override
	public void setIdentityService(IIdentityService identity) {
		//don't consider identity
	}

}
