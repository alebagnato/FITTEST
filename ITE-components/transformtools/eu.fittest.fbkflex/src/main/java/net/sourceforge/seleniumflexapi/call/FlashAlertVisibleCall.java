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

public class FlashAlertVisibleCall implements FlashCall {

	private final FlexSelenium flashApp;
	private final boolean checkVisibleOrInvisible;

	public FlashAlertVisibleCall(FlexSelenium flashApp, boolean checkVisibleOrInvisible) {
		this.flashApp = flashApp;
		this.checkVisibleOrInvisible = checkVisibleOrInvisible;
	}


	public boolean attemptCall() {
		return flashApp.isAlertVisible() == checkVisibleOrInvisible;
	}


	public String getErrorMessage() {
		
		if (!checkVisibleOrInvisible) {
			return "Alert still visible";
		}			
		
		return "Alert not visible";
	}	
		
}
