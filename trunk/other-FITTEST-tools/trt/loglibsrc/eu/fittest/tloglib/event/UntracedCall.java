package eu.fittest.tloglib.event;

import java.lang.reflect.* ;
import eu.fittest.tloglib.error.*;

public class UntracedCall extends Event {
	
	/**
	 * The name of the called method. Or rather, the name of its decoder. Format:
	 * 
	 *     pckName!className!dec_decodername
	 *     
	 *  pckName is fully qualified.
	 */
	public String name ;

	public static final int CODE = 3 ;
	
	public UntracedCall(int time, String name) {
		this.time = time ; this.name = name ;
	}
	
	// extract the fully qualified package-name
	private String getPackageName() {
		int k = name.indexOf('!') ;
		return name.substring(0,k) ;
	}
	
    // extract class name
	private String getClassName() {
		int k1 = name.indexOf('!') ;
		String s2 = name.substring(k1 + 1) ;
		int k2 = s2.indexOf('!') ;
		return s2.substring(0,k2) ;
	}

    // extract decoder name
	private String getDecoderName() {
		int k3 = name.lastIndexOf('!') ;
		String s3 = name.substring(k3 + 1) ;
		return s3 ;
	}
	
	@Override
	public void replay() throws Exception {
		String fullClassName = "" ;
		if (! getPackageName().equals("")) fullClassName += getPackageName() + "." ;
		fullClassName += getClassName() ;
		
		Method decoder ;
		//System.out.println("###1" + name) ;
			Class decClass;
			try {
				//System.out.println("** class name = " + fullClassName ) ;
				//System.out.println("** decoder name = " + getDecoderName() ) ;
				decClass = Class.forName(fullClassName) ;
				decoder = decClass.getDeclaredMethod(getDecoderName()) ;
				
			} catch (Exception e) {
				throw new LogDecodingError("Fail to replay an untraced call " + name + ", problem: " + e) ;
			} 
			
			try {
				decoder.invoke(null) ; // decoder is a static method
			} 
			catch (InvocationTargetException e) {
				// the invoked method throws an exception; rethrow it:
				Throwable e_ = e.getCause() ;
				if (e_ instanceof Exception)
					 throw (Exception) e_ ;
				else throw (Error) e_ ;
			} 
			catch(Exception f) {
				throw new LogDecodingError("Fail to replay an untraced call " + name + ", problem: " + f) ;
			}
	}

	@Override
	public String serialize() {
		return ":" + time + ":" + CODE + ":" + name ;
	}

}
