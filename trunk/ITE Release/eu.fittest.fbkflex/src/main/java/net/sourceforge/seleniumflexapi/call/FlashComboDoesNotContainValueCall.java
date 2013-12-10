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

public class FlashComboDoesNotContainValueCall implements FlashCall {

	private String objectId;
	private FlexSelenium flashApp;
	private String expectedValue;

	public FlashComboDoesNotContainValueCall(FlexSelenium flashApp, String objectId, String expectedValue) {
		super();
		this.flashApp = flashApp;
		this.objectId = objectId;
		this.expectedValue = expectedValue;
	}

	
	public boolean attemptCall() {		
		return !flashApp.isLabelInComboData(objectId, expectedValue);
	}

	
	public String getErrorMessage() {
		return "The selected item '" + expectedValue + "' for " + objectId + " existed";
	}

}
