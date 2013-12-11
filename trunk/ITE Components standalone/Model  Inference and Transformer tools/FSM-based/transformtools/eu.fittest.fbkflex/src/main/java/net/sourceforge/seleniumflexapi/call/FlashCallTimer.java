/*	
 *	License
 *	
 *	This file is part of The SeleniumFlex-API.
 *	
 *	The SeleniumFlex-API is free software: you can redistribute it and/or
 *  modify it  under  the  terms  of  the  GNU  General Public License as 
 *  published  by  the  Free  Software Foundation,  either  version  3 of 
 *  the License, or any later version.
 *
 *  The SeleniumFlex-API is distributed in the hope that it will be useful,
 *  but  WITHOUT  ANY  WARRANTY;  without  even the  implied  warranty  of
 *  MERCHANTABILITY   or   FITNESS   FOR  A  PARTICULAR  PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with The SeleniumFlex-API.
 *	If not, see http://www.gnu.org/licenses/
 *
 */
 
 /*   Contributed by Black Pepper Software Ltd.  */
 
 
package net.sourceforge.seleniumflexapi.call;


import org.junit.Assert;

public class FlashCallTimer {

	private static final long INCREMENT = 250L;

	private FlashCallTimer() {
		// static helper
	}

	public static void waitForCall(final long timeoutMillis, final FlashCall call) {
		final long endTime = System.currentTimeMillis() + timeoutMillis;
		do {
			if (call.attemptCall()) {
				return;
			}
			
			try {
				Thread.sleep(INCREMENT);
			}
			catch (InterruptedException e) {
				// We can ignore this.  The worst that can happen is that we
				// try the call more often than the defined interval
			}
		}
		while (System.currentTimeMillis() <= endTime);
		
		Assert.fail(String.format("Timed out after %dms: %s", timeoutMillis, call.getErrorMessage()));
	}

	public static void waitForNegatedCall(final long timeout, final FlashCall call) {
		waitForCall(timeout, new FlashCall() {
			
			public boolean attemptCall() {
				return ! call.attemptCall();
			}

			
			public String getErrorMessage() {
				return "NOT " + call.getErrorMessage();
			}			
		});
	}

}
