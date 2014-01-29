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

public class FlashDataGridRowLabelVisibleCall implements FlashCall {

	private final String dataGridId;
	private final String field;
	private final String label;
	private final FlexSelenium flashApp;
	private boolean visible;

	public FlashDataGridRowLabelVisibleCall(FlexSelenium flashApp, String dataGridId, String field, String label, boolean visible) {
		this.flashApp = flashApp;
		this.dataGridId = dataGridId;
		this.field = field;
		this.label = label;
		this.visible = visible;
	}

	
	public boolean attemptCall() {
		int row = flashApp.getFlexDataGridRowIndexForFieldLabel(dataGridId, field, label);
		if (visible) {
			return row > -1;
		} else {
			return row == -1;
		}
	}

	
	public String getErrorMessage() {
		
		if (!visible) {
			return dataGridId + " still showing row with field " + field + " = " + label;
		}			
		
		return dataGridId + " not showing row with field " + field + " = " + label;
	}	
		
}
