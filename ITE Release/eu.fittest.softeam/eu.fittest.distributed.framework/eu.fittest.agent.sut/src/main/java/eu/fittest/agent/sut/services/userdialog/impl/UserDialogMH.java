package eu.fittest.agent.sut.services.userdialog.impl;

import eu.fittest.agent.sut.services.userdialog.spec.IUserDialogService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.UserDialog;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class UserDialogMH extends AbstractMessageHandler<IUserDialogService> {

	protected UserDialogMH(IUserDialogService service) {
		super(service);
	}
	
	public synchronized void onReception(Connection connection, UserDialog message)	throws FITTESTException {
		_service.printMessage(message.getMessage(), message.getType());
	}

}
