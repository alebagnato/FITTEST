package eu.fittest.agent.sut.services.ite.spec;

import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.services.registration.spec.RegistrationData;

public class ITERegistrationEvent extends
		ServiceEvent<ILocalITERegistrationService> {
	private RegistrationData _data;
	private ITERegistrationEventKind _kind;

	public ITERegistrationEventKind getKind() {
		return _kind;
	}

	public ITERegistrationEvent(ILocalITERegistrationService source, RegistrationData data, ITERegistrationEventKind kind) {
		super(source);
		_data = data;
		_kind = kind;
	}

	public RegistrationData getData() {
		return _data;
	}
}
