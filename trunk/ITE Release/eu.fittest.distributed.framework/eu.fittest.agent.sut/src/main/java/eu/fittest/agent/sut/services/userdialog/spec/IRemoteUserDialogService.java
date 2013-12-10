package eu.fittest.agent.sut.services.userdialog.spec;

import eu.fittest.common.core.service.IRemoteService;

public interface IRemoteUserDialogService extends IRemoteService {
	void printMessage(String message, int type);
}
