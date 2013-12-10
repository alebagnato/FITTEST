package eu.fittest.common.util;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Enumeration;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.common.core.exception.FITTESTException;

public class NetworkUtils {
	static public String getLocalIP() throws FITTESTException{
		try {
	    	String result = null;
	        Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
			while (result == null && networkInterfaces.hasMoreElements()) { 
	            NetworkInterface netInterface = networkInterfaces.nextElement(); 
	            ITELogger.log(Level.FINEST,netInterface.getName());
	            Enumeration<InetAddress> addresses = netInterface.getInetAddresses(); 
	            while (result == null && addresses.hasMoreElements()) { 
	                InetAddress ip = addresses.nextElement(); 
	                if (ip instanceof Inet4Address && !ip.isLoopbackAddress()) {
	                	ITELogger.log(Level.FINEST,netInterface.getName());
	                	result = ip.getHostAddress();
	                } 
	            } 
	        }
			if(result==null) result="localhost";
			return result;
		} catch (SocketException e) {
			throw new FITTESTException(e.getMessage());
		} 
        
    }
}
