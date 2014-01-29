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

public class FlashTextValueChangedCall implements FlashCall {

	private final String oldValue;
	private final FlexSelenium flashApp;
	private final String fieldId;
	
	
	public FlashTextValueChangedCall(final FlexSelenium flashApp, final String fieldId, final String oldValue) {
		this.flashApp = flashApp;
		this.fieldId = fieldId;
		this.oldValue = oldValue;
	}
	
	
	public boolean attemptCall() {
		String currentValue = flashApp.getText(fieldId);
		return !currentValue.equals(oldValue);
	}

	
	public String getErrorMessage() {
		return "The value of " + fieldId + " has not changed originally " + oldValue + " currently " + flashApp.getText(fieldId);
	}

}
