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
 *
 *	Modified for FITTEST project by CuND/FBK
 *	Date: May 2011
 *
 */
 
 /*   Contributed by Black Pepper Software Ltd.  */
 
 
package eu.fittest.flexdriver;


import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

import net.sourceforge.seleniumflexapi.FlexSelenium;
import net.sourceforge.seleniumflexapi.call.FlashAlertVisibleCall;
import net.sourceforge.seleniumflexapi.call.FlashCall;
import net.sourceforge.seleniumflexapi.call.FlashCallTimer;
import net.sourceforge.seleniumflexapi.call.FlashCheckboxCheckedCall;
import net.sourceforge.seleniumflexapi.call.FlashComboContainsValueCall;
import net.sourceforge.seleniumflexapi.call.FlashComboDoesNotContainValueCall;
import net.sourceforge.seleniumflexapi.call.FlashComboHasSelectedValueCall;
import net.sourceforge.seleniumflexapi.call.FlashDataGridRowHasLabelCall;
import net.sourceforge.seleniumflexapi.call.FlashDataGridRowHasValueCall;
import net.sourceforge.seleniumflexapi.call.FlashDataGridRowLabelVisibleCall;
import net.sourceforge.seleniumflexapi.call.FlashDataGridRowVisibleCall;
import net.sourceforge.seleniumflexapi.call.FlashFieldHasErrorMessageCall;
import net.sourceforge.seleniumflexapi.call.FlashNumberOfRowsCall;
import net.sourceforge.seleniumflexapi.call.FlashNumberOfRowsSelectedCall;
import net.sourceforge.seleniumflexapi.call.FlashPropertySetCall;
import net.sourceforge.seleniumflexapi.call.FlashTabEnabledCall;
import net.sourceforge.seleniumflexapi.call.FlashTextHasValueCall;
import net.sourceforge.seleniumflexapi.call.FlashTextValueChangedCall;

import org.junit.Assert;


public class FlexObjectDriver {
	private static final String ACTION_SUCCESS = "true";
	private static final String WIDGET_PROPERTY_DATA_PROVIDER = "dataProvider";
	private static final long TIMEOUT_MILLIS = 10000L;
	
	private final FlexSelenium flexSelenium;

	public FlexObjectDriver(final FlexSelenium flexSelenium) {
		this.flexSelenium = flexSelenium;
	}
	
	public FlexSelenium getFlexSelenium() {
		return flexSelenium;
	}
	
	public void setFocus(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);

		Assert.assertEquals(ACTION_SUCCESS, flexSelenium.setFocus(widgetId));
	}
	
	public void clickButton(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		Assert.assertEquals(ACTION_SUCCESS, flexSelenium.click(widgetId));
	}
	
	public void setCheckBoxState(final String widgetId, final boolean state) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		Assert.assertEquals(ACTION_SUCCESS, flexSelenium.selectCheckbox(widgetId, state));
	}

	public void ensureWidgetEnabled(final String widgetId, final boolean enabledState) {
		ensureWidgetEnabled(widgetId, enabledState, TIMEOUT_MILLIS);
	}

	public void ensureWidgetEnabled(final String widgetId, final boolean enabledState, final long timeoutMillis) {
		// This mechanism for checking the enabled state is to work around an intermittent issue where the widget
		// does not exist at the time of the enabled check.
		FlashCallTimer.waitForCall(timeoutMillis, new FlashCall() {

			public boolean attemptCall() {
				try {
					return flexSelenium.isEnabled(widgetId) == enabledState;
				}
				catch (Throwable t) {
					System.out.println(String.format("Enabled state of '%s' threw an Exception: %s", widgetId, t.getMessage()));
					return false;
				}
			}


			public String getErrorMessage() {
				return String.format("The enabled state of widget %s was not %b", widgetId, enabledState);
			}
		});
	}

	public void ensureWidgetVisibility(final String widgetId, final boolean visibility) {
		ensureWidgetVisibility(widgetId, visibility, TIMEOUT_MILLIS);
	}
	
	public void ensureWidgetVisibility(final String widgetId, final boolean visibility, final long timeoutMillis) {
		// This mechanism for checking the visibility state is to work around an intermittent issue where the widget
		// does not exist at the time of the visibility check.
		FlashCallTimer.waitForCall(timeoutMillis, new FlashCall() {

			public boolean attemptCall() {
				try {
					boolean isVisible = flexSelenium.isVisible(widgetId); 
					return visibility == isVisible;
				}
				catch (Throwable t) {
					System.out.println(String.format("Visibility of '%s' threw an Exception: %s", widgetId, t.getMessage()));
					return false;
				}
			}


			public String getErrorMessage() {
				return String.format("The visibility of widget %s was not %b", widgetId, visibility);
			}
		});
	}

	public void ensureCheckBoxState(final String widgetId, final boolean expectedState) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashCheckboxCheckedCall(flexSelenium, widgetId, expectedState));
	}

	public void enterText(final String widgetId, final String text) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		flexSelenium.enterText(widgetId, text);
	}
	
	public void enterText(final String widgetId, final Object text) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		flexSelenium.enterText(widgetId, text.toString());
	}
	
	public void enterText(final String widgetId, final long text) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		flexSelenium.enterText(widgetId, String.valueOf(text));
	}

	public void enterText(final String widgetId, final double text) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		flexSelenium.enterText(widgetId, String.valueOf(text));
	}

	public void enterText(final String widgetId, final int text) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		flexSelenium.enterText(widgetId, String.valueOf(text));
	}

	public void enterDate(final String widgetId, final Calendar date, final String format) {
		final DateFormat formatter = new SimpleDateFormat(format);
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		flexSelenium.enterDate(widgetId, formatter.format(date.getTime()));
	}

	public void ensureTabEnabled(final String widgetId, final String tabLabel, final boolean enabled) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashTabEnabledCall(flexSelenium, widgetId, tabLabel, enabled));
	}
	
	public void selectTab(final String widgetId, final String tabTitle) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		Assert.assertEquals(ACTION_SUCCESS, flexSelenium.click(widgetId, tabTitle));
	}
	
	public void selectComboItem(final String widgetId, final String label) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashPropertySetCall(flexSelenium, widgetId, "dataProvider"));
		
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashComboContainsValueCall(flexSelenium, widgetId, label));

		Assert.assertEquals("true", flexSelenium.selectComboByLabel(widgetId, label));

		ensureComboSelectedItem(widgetId, label);
	}
	
	public void ensureComboSelectedItem(final String widgetId, final String value) {
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashComboHasSelectedValueCall(flexSelenium, widgetId, value));
	}

	public void ensureComboNoSelectedItem(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashCall() {

			public boolean attemptCall() {
				return flexSelenium.getSelectionIndex(widgetId) == -1;
			}


			public String getErrorMessage() {
				return String.format("Combo box %s has a selected item", widgetId);
			}
		});
	}

	public void ensureComboSelectedItemNotAvailable(
			final String widgetId, final String value) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashComboDoesNotContainValueCall(flexSelenium, widgetId, value));
	}

	public void ensureComboContainsValue(final String widgetId, final String value) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashComboContainsValueCall(flexSelenium, widgetId, value));
	}
	
	public void selectMultiChoiceList(final String widgetId, final String[] values) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashPropertySetCall(flexSelenium, widgetId, WIDGET_PROPERTY_DATA_PROVIDER));

		for (final String value : values) {
			Assert.assertEquals(ACTION_SUCCESS, flexSelenium.select(widgetId, value));
		}
	}
	
	public void selectByIndices(final String widgetId, final int[] indices) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		ensureDataGridPopulated(widgetId);

		for (final int index : indices) {
			flexSelenium.selectIndex(widgetId, index);
		}
	}

	public void ensureDataGridRowVisible(final String widgetId, final String property, final String value, final boolean visible) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashDataGridRowVisibleCall(flexSelenium, widgetId, property, value, visible));
	}

	public void ensureDataGridRowWithLabelVisible(final String widgetId, final String property, final String label, final boolean visible) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashDataGridRowLabelVisibleCall(flexSelenium, widgetId, property, label, visible));
	}

	public void ensureDataGridRowValue(final String widgetId, final int row, final String property, final String expectedValue) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashDataGridRowHasValueCall(flexSelenium, widgetId, row, property, expectedValue));
	}

	public void ensureDataGridRowLabel(final String widgetId, final int row, final String property, final String expectedValue) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetEnabled(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashDataGridRowHasLabelCall(flexSelenium, widgetId, row, property, expectedValue));
	}

	public void ensureWidgetHasText(final String widgetId, final String expectedText) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashTextHasValueCall(flexSelenium, widgetId, expectedText));
	}
	
	public void ensureWidgetHasMatchingText(final String widgetId, final String regularExpression) {
		ensureWidgetVisibility(widgetId, true);
		
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashCall() {

			public boolean attemptCall() {
				return flexSelenium.getText(widgetId).matches(regularExpression);
			}


			public String getErrorMessage() {
				return String.format("The text of %s did not meet the regular expression '%s'", widgetId, regularExpression);
			}
		});
	}

	public void ensureWidgetTextNotBlank(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForNegatedCall(TIMEOUT_MILLIS, new FlashTextHasValueCall(flexSelenium, widgetId, ""));
	}

	public void ensureWidgetTextChanged(final String widgetId, final String initialValue) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashTextValueChangedCall(flexSelenium, widgetId, initialValue));
	}

	public void ensureWidgetHasDate(final String widgetId, final Calendar date, final String format) {
		final DateFormat formatter = new SimpleDateFormat(format);
		final String expectedText = date != null ? formatter.format(date.getTime()) : ""; 

		ensureWidgetHasText(widgetId, expectedText);
	}
	
	public void ensureDataGridPopulated(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);
		ensureWidgetPropertySet(widgetId, WIDGET_PROPERTY_DATA_PROVIDER);
	}

	public void ensureDataGridRowCount(final String widgetId, final int expectedRowCount) {
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashNumberOfRowsCall(flexSelenium, widgetId, expectedRowCount));
	}
	
	public void ensureDataGridRowCountChanged(final String widgetId, final int initialRowCount) {
		FlashCallTimer.waitForNegatedCall(TIMEOUT_MILLIS, new FlashNumberOfRowsCall(flexSelenium, widgetId, initialRowCount));
	}
	
	public void ensureDataGridRowValue(final String widgetId, final int row, final int column, final String expectedValue) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashCall() {
			private String lastValue;
			

			public boolean attemptCall() {
				lastValue = flexSelenium.getDataGridValue(widgetId, row, column);
				return expectedValue.equals(lastValue);
			}


			public String getErrorMessage() {
				return String.format("%s[%d, %d] did not have value '%s', last value was '%s'", widgetId, row, column, expectedValue, lastValue);
			}
		});
	}

	public void ensureWidgetErrorString(final String widgetId, final String expectedMessage) {
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashFieldHasErrorMessageCall(flexSelenium, widgetId, expectedMessage));
	}
	
	public int getDataGridRowIndex(final String widgetId, final String field, final String value) {
		ensureDataGridRowVisible(widgetId, field, value, true);

		return flexSelenium.getFlexDataGridRowIndexForFieldValue(widgetId, field, value);
	}
	
	public int getDataGridRowIndexByLabel(final String widgetId, final String field, final String value) {
		ensureDataGridRowWithLabelVisible(widgetId, field, value, true);

		return flexSelenium.getFlexDataGridRowIndexForFieldLabel(widgetId, field, value);
	}

	public String getDataGridRowValue(final String widgetId, final String field, final int row) {
		Assert.assertTrue("Not enough rows in the table", getDataGridRowCount(widgetId) > row);

		return flexSelenium.getFlexDataFieldValueForGridRow(widgetId, field, row);
	}
	
	public int getDataGridRowCount(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashPropertySetCall(flexSelenium, widgetId, WIDGET_PROPERTY_DATA_PROVIDER));
		
		return flexSelenium.getFlexDataGridRowCount(widgetId);
	}
	
	public void ensureAlertVisibility(final boolean visible) {
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashAlertVisibleCall(flexSelenium, visible));
	}
	
	public void closeAlert(final String response) {
		ensureAlertVisibility(true);
		Assert.assertEquals(ACTION_SUCCESS, flexSelenium.call("doFlexAlertResponse", response, response));
		ensureAlertVisibility(false);
	}
	
	/**
	 * Select an element from a list.
	 * 
	 * @param widgetId the id of the widget from which to select
	 * @param field the field (member) of the object to use for selection
	 * @param value the data value to look for in the field to find the object to select
	 */
	public void selectByFieldValue(final String widgetId, final String field, final String value) {
//		ensureDataGridRowVisible(widgetId, field, value, true);
		Assert.assertEquals(ACTION_SUCCESS, flexSelenium.selectMatchingOnField(widgetId, field, value));
	}

	public String getWidgetText(final String widgetId) {
		ensureWidgetVisibility(widgetId, true);

		return flexSelenium.getText(widgetId);
	}
	
	public void selectMultipleDataGridRows(final String widgetId, final String property, final String[] values) {
		ensureWidgetVisibility(widgetId, true);
		ensureDataGridPopulated(widgetId);
		
		int selectedCount = 0;
		for (final String value : values) {
			flexSelenium.addSelectMatchingOnField(widgetId, property, value);
			selectedCount++;
			
			// Wait for the selection to be made, otherwise we may override it
			FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashNumberOfRowsSelectedCall(flexSelenium, widgetId, selectedCount));
		}
	}
	
	public void ensureSelectedListItems(final String widgetId, final String[] expectedItems) {
		ensureWidgetVisibility(widgetId, true);
		
		final int numSelected = flexSelenium.getNumSelectedItems(widgetId);

		Assert.assertEquals("Wrong number of items selected", expectedItems.length, numSelected);

		final List<String> expectedItemsList = Arrays.asList(expectedItems);
		final List<String> actualItemsList = new ArrayList<String>();
		for (int i = 0; i < numSelected; i++) {
			final String selectedItem = flexSelenium.getSelectedItemAtIndex(widgetId, i);
			
			actualItemsList.add(selectedItem);
		}
		
		Collections.sort(expectedItemsList);
		Collections.sort(actualItemsList);
		
		Assert.assertEquals(expectedItemsList, actualItemsList);
	}
	
	public void captureScreenShot(final String fileName) {
		flexSelenium.captureScreenshot("./build/" + fileName + ".png");
	}
	
	public void ensureWidgetPropertySet(final String widgetId, final String property) {
		FlashCallTimer.waitForCall(TIMEOUT_MILLIS, new FlashPropertySetCall(flexSelenium, widgetId, property));
	}
}

