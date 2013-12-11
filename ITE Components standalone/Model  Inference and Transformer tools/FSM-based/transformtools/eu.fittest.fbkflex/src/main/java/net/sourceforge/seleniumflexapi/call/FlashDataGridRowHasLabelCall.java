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

public class FlashDataGridRowHasLabelCall implements FlashCall {

	private String field;
	private String dataGridId;
	private FlexSelenium flashApp;
	private String expectedValue;
	private int row;
	private String value;

	public FlashDataGridRowHasLabelCall(FlexSelenium flashApp, String dataGridId, int row, String field, String expectedValue) {
		super();
		this.flashApp = flashApp;
		this.dataGridId = dataGridId;
		this.field = field;
		this.row = row;
		this.expectedValue = expectedValue;
	}

	
	public boolean attemptCall() {
		value = flashApp.getFlexDataFieldLabelForGridRow(dataGridId, field, row);
		if (value == null || !value.equals(expectedValue)) {
			return false;
		}
		return true;
	}

	
	public String getErrorMessage() {
		return dataGridId + "." + field  + " did not have label '" + expectedValue + "' but was '" + value + "'";
	}

}
