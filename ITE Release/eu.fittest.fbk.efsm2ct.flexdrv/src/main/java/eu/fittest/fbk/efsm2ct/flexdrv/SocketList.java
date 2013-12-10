/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import eu.fittest.fbk.efsm2ct.flexdrv.logging.LoggerManager;

/**
 *
 * @author tiella
 */
public class SocketList {

    private static Logger log = Logger.getLogger(LoggerManager.LOGGER);
    private List<Socket> sockets = new ArrayList<Socket>();
    private List<ServerSocket> serversockets = new ArrayList<ServerSocket>();

    public synchronized boolean add(ServerSocket e) {
        return serversockets.add(e);
    }

    public synchronized boolean add(Socket e) {
        return sockets.add(e);
    }

    public synchronized void closeAll() {

        for (Socket s : sockets) {

            if (s.isBound() || s.isConnected()) {
                log.warning("Socket:" + s + " found bound or connected, closing it.");

                try {
                    s.close();

                } catch (Throwable ex) {
                    log.warning("error: " + ex + " closing socket:" + s);
                }
            }
        }

        for (ServerSocket s : serversockets) {

            if (s.isBound()) {
                log.warning("ServerSocket:" + s + " found bound, closing it.");

                try {
                    s.close();

                } catch (Throwable ex) {
                    log.warning("error: " + ex + " closing socket:" + s);
                }
            }

        }

        sockets.clear();
        serversockets.clear();

    }
}
