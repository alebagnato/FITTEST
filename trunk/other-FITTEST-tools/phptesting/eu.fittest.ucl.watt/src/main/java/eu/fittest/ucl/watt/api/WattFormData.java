/*****************************************************************************************************************************************************************************************************************/
Copyright (c) 2010-2050, UCL. All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
/*****************************************************************************************************************************************************************************************************************/

package eu.fittest.ucl.watt.api;

public class WattFormData {
	private String formName;
	private String inputName;
	private String inputType;
	private String inputValue;
	
	public WattFormData() {
		this.formName = this.inputName = this.inputType = this.inputValue = null;
	}

	public String getFormName() {
		return formName;
	}

	public void setFormName(String formName) {
		this.formName = formName;
	}

	public String getInputName() {
		return inputName == null? "":inputName;
	}

	public void setInputName(String inputName) {
		this.inputName = inputName;
	}

	public String getInputType() {
		return inputType == null? "":inputType;
	}

	public void setInputType(String inputType) {
		this.inputType = inputType;
	}

	public String getInputValue() {
		return inputValue == null? "":inputValue;
	}

	public void setInputValue(String inputValue) {
		this.inputValue = inputValue;
	}
}
