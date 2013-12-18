package eu.fittest.tloglib.event;

import eu.fittest.tloglib.* ;

/**
 * Used to log dynamic value.
 * Author: Wishnu Prasetya
 */
public class DynamicValueEvent extends Event {
	
	public String value ;
	
	public static final int CODE = 2 ;
	
	public DynamicValueEvent(int time, String value) {
		this.time = time ; this.value = value ;
	}

	@Override
	public void replay() throws Exception {
		DLog.xwrite(" <" + value + ">") ;
		DLog.write("") ;
	}

	@Override
	public String serialize() {
		return ":" + time + ":" + CODE + ":" + value ;
	}

}
