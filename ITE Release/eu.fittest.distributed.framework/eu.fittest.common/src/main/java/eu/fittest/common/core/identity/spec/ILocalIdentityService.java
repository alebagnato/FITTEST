package eu.fittest.common.core.identity.spec;

import eu.fittest.common.core.service.ILocalService;

public interface ILocalIdentityService extends ILocalService{
	String getMyIdentity();
	void setMyIdentity(FITTESTEntity entity);
}
