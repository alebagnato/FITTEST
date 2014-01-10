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

import net.sourceforge.seleniumflexapi.FlexSelenium;

public class FlashNumberOfRowsCall implements FlashCall {

	private final String listId;
	private final FlexSelenium flashApp;
	private final int expectedRowCount;
	private int lastRealRowCount;

	public FlashNumberOfRowsCall(FlexSelenium flashApp, String listId, int expectedRowCount) {
		super();
		this.flashApp = flashApp;
		this.listId = listId;
		this.expectedRowCount = expectedRowCount;
	}

	
	public boolean attemptCall() {
		lastRealRowCount = flashApp.getFlexDataGridRowCount(listId);
		return lastRealRowCount == expectedRowCount;
	}

	
	public String getErrorMessage() {		
		return String.format("%s does not have %d rows, but had %d rows", listId, expectedRowCount, lastRealRowCount);
	}
}
