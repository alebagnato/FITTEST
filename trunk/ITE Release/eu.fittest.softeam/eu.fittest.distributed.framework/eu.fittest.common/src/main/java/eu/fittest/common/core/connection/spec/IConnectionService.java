package eu.fittest.common.core.connection.spec;



import eu.fittest.common.core.server.spec.IServerServiceListener;
import eu.fittest.common.core.service.IService;


public interface IConnectionService extends IService, ILocalConnectionService, IServerServiceListener {

}
