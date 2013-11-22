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

public class FlashTabEnabledCall implements FlashCall {

	private final String objectId;
	private final String tabLabel;
	private final FlexSelenium flashApp;
	private final boolean checkEnabledOrDisabled;

	public FlashTabEnabledCall(final FlexSelenium flashApp, final String objectId, final String tabLabel) {
		this(flashApp, objectId, tabLabel, true);
	}
	
	public FlashTabEnabledCall(final FlexSelenium flashApp, final String objectId, final String tabLabel, final boolean checkEnabledOrDisabled) {
		super();
		this.flashApp = flashApp;
		this.objectId = objectId;
		this.checkEnabledOrDisabled = checkEnabledOrDisabled;
		this.tabLabel = tabLabel;
	}

	
	public boolean attemptCall() {
		return flashApp.isTabEnabled(objectId, tabLabel) == checkEnabledOrDisabled;
	}

	
	public String getErrorMessage() {
		final String widgetName = String.format("Tab '%s' of %s", tabLabel, objectId);

		if (!checkEnabledOrDisabled) {
			return widgetName + " still enabled";
		}			
		
		return widgetName + " not enabled";
	}	
		
}
