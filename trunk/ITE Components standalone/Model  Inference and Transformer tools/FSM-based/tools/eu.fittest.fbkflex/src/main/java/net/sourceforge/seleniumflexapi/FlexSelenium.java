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
 
 
package net.sourceforge.seleniumflexapi;

import com.thoughtworks.selenium.FlashSelenium;
import com.thoughtworks.selenium.Selenium;

public class FlexSelenium extends FlashSelenium {

	private Selenium selenium;
	
	public FlexSelenium(Selenium selenium, String flashObjectId) {
		super(selenium, flashObjectId);
		this.selenium = selenium;
	}

	public String click(String objectId, String optionalButtonLabel) {
		return call("doFlexClick", objectId, optionalButtonLabel);
	}

	public String click(String objectId) {
		return click(objectId, "");
	}

	public boolean isVisible(String objectId) {
		final String result = call("getFlexVisible", objectId, "");
		
		if (!"true".equals(result) && !"false".equals(result)) {
			System.out.println(String.format("Visibility of '%s' returned an unexpected value: %s", objectId, result));
		}
		
		return Boolean.parseBoolean(result);
	}

	public int getNumSelectedItems(String objectId) {
		return Integer.parseInt(call("getFlexNumSelectedItems", objectId, ""));
	}
	
	public String getSelectedItemAtIndex(String objectId, int index) {
		return call("getFlexSelectedItemAtIndex", objectId, Integer.toString(index));
	}

	public void enterText(String textField, String text) {
		call("doFlexType", textField, text);		
	}

	public void enterDate(String dateField, String dateAsText) {
		call("doFlexDate", dateField, dateAsText);		
	}
	
	public void getDate(String dateField) {
		call("getFlexDate", dateField, "");
	}

	public boolean isEnabled(String objectId) {
		final String state = call("getFlexEnabled", objectId, "");
		
		if (!"true".equals(state) && !"false".equals(state)) {
			System.out.println(String.format("Enabled state of '%s' returned an unexpected value: %s", objectId, state));
		}
		return Boolean.parseBoolean(state);
	}
	
	public boolean isTabEnabled(String objectId, String label) {
		return Boolean.parseBoolean(call("getFlexEnabled", objectId, label));
	}
	
	public boolean exists(String objectId) {
		return Boolean.parseBoolean(call("getFlexExists", objectId, ""));
	}

	public String getErrorString(String textFieldId) {
		return call("getFlexErrorString", textFieldId, "");
	}
	
	public String getText(String textFieldId) {
		return call("getFlexText", textFieldId, "");
	}

	public int getSelectionIndex(String selectionFieldId) {
		return Integer.parseInt(call("getFlexSelectionIndex", selectionFieldId, ""));
	}

	public String select(String selectionFieldId, String itemToSelect) {
		return call("doFlexSelect", selectionFieldId, itemToSelect);
	}
	
	public String selectComboByLabel(String selectionFieldId, String itemToSelect) {
		return call("doFlexSelectComboByLabel", selectionFieldId, itemToSelect);
	}

	public String selectMatchingOnField(String selectionFieldId, String underlyingField, String underlyingValue) {
		return call("doFlexSelectMatchingOnField", selectionFieldId, underlyingField, underlyingValue);
	}

	public void addSelectMatchingOnField(String selectionFieldId, String underlyingField, String underlyingValue) {
		String data = call("doFlexAddSelectMatchingOnField", selectionFieldId, underlyingField, underlyingValue);
		System.out.println("Result: " + data);
	}
	
	public void selectIndex(String selectionFieldId, int index) {
		call("doFlexSelectIndex", selectionFieldId, Integer.toString(index));
	}

	public String getProperty(String objectId, String property) {
		return call("getFlexProperty", objectId, property);
	}
	
	public String getDataGridValue(String dataGridId, int row, int col) {
		return call("getFlexDataGridCell", dataGridId, Integer.toString(row), Integer.toString(col));
	}
	
	public int getFlexDataGridRowIndexForFieldValue(String dataGridId, String field, String value) {
		return Integer.parseInt(call("getFlexDataGridRowIndexForFieldValue", dataGridId, field, value));
	}
	
	public int getFlexDataGridRowIndexForFieldLabel(String dataGridId, String field, String label) {
		return Integer.parseInt(call("getFlexDataGridRowIndexForFieldLabel", dataGridId, field, label));
	}

	public String getFlexDataFieldValueForGridRow(
			String dataGridId, String field, int row) {
		return call("getFlexDataGridFieldValueForGridRow", dataGridId, field, Integer.toString(row));
	}

	public String getFlexDataFieldLabelForGridRow(
			String dataGridId, String field, int row) {
		return call("getFlexDataGridFieldLabelForGridRow", dataGridId, field, Integer.toString(row));
	}

	public int getFlexDataGridRowCount(String dataGridId) {
		return Integer.parseInt(call("getFlexDataGridRowCount", dataGridId));
	}

	public boolean isAlertVisible() {
		return Boolean.parseBoolean(call("getFlexAlertPresent", "", ""));
	}
	
	public String clickAlert(String alertButton) {
		return call("doFlexAlertResponse", alertButton, alertButton);
	}
	
	public String selectCheckbox(String checkBoxId, boolean value) {
		return call("doFlexCheckBox", checkBoxId, Boolean.toString(value));
	}

	public boolean isCheckboxChecked(String checkBoxId) {
		return Boolean.parseBoolean(call("getFlexCheckBoxChecked", checkBoxId, ""));
	}

	public String getComboBoxSelectedItem(String comboBoxId) {
		return call("getFlexSelection", comboBoxId, "");
	}

	public void captureScreenshot(String saveToFile) {
		selenium.captureScreenshot(saveToFile);
	}
	
	public boolean isLabelInComboData(final String comboBoxId, final String expectedLabel) {
		return "true".equals(call("getFlexComboContainsLabel", comboBoxId, expectedLabel));
	}
	
	public String setFocus(final String widgetId) {
		return call("doFlexSetFocus", widgetId);
	}
	
}
