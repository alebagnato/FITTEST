package eu.fittest.tloglib.event;

import java.lang.reflect.*;

import eu.fittest.tloglib.DLog;
import eu.fittest.tloglib.error.*;

public class ExceptionEvent extends Event {
	
	/**
	 * The name of the exception.
	 */
	public String name  ;
	
	public static final int CODE = 1 ;
	
	public ExceptionEvent(int time, String name) {
		this.time = time ; this.name = name ;
	}

	@Override
	public void replay() throws Exception {
		Constructor C;
		Throwable e ;
		try {
			//System.out.println("###1" + name) ;
			C = Class.forName(name).getConstructor() ;	
			//System.out.println("###2" + C.getName()) ;
			e = (Throwable) C.newInstance() ;
		} catch (Exception f) {
			throw new LogDecodingError("Fail to replay the exception " + name + ", problem: " + f) ;
		}
		DLog.writeln("EXCEPTION: " + name) ;
		if (e instanceof Exception) throw (Exception) e ;	
		if (e instanceof Error) throw (Error) e ;
		throw new LogDecodingError("Fail to replay the Throwable: " + name + " because it is neither an instance of Exception nor af Error." ) ;
	} 

	@Override
	public String serialize() {
		return ":" + time + ":" + CODE + ":" + name ;
	}

	public static void main (String[] args) throws Exception {
		Exception e = new java.io.IOException() ;
		String cname = e.getClass().getName() ;
		System.out.println("** " + cname) ;
		Class C = Class.forName("java.io.IOException") ;
		Constructor con = C.getConstructor() ;
		Exception e2 = (Exception) C.newInstance() ;
		System.out.println("** " + e2) ;
	}

}
