package eu.fittest.common.core.connection.spec;

import java.util.List;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.service.ILocalService;
import eu.fittest.common.core.service.IMessageHandler;
import eu.fittest.common.core.xml.Message;

public interface ILocalConnectionService extends ILocalService {

    void registerMessageHandler(final IMessageHandler handler);

    void sendMessage(final Message message) throws FITTESTException;
    
    void broadcastMessage(Connection fromConnection, Message message) throws FITTESTException;

    void map(final String id, final Connection connection);
    void unmap(final String id);

    Connection getConnection(final String IP, final int port);
    
    Connection getConnection(final String id);

    void addConnection(final Connection connection);
    
    List<String> getReachableId(Connection connection);
    
    void stop();

}
