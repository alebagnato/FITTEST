/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.flexdrv;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 
 * @author tiella
 */
public class LogParser {
	// typical instance
	//
	// instance type:AppAbstractState
	// attr:catalogContents
	// type:Array
	// value:eu.fittest.modelInference.logConverter.model.Instance@6345e044
	// attr:cartTotal
	// type:String
	// value:"$0.00"
	// attr:numInShopCart
	// type:int
	// value:0
	// attr:compareCartContents
	// type:Array
	// value:eu.fittest.modelInference.logConverter.model.Instance@86c347
	// attr:shoppingCartContents
	// type:Array
	// value:eu.fittest.modelInference.logConverter.model.Instance@f7e6a96
	// attr:numInCompareCart
	// type:int
	// value:0
	// attr:numOfSelectedItems
	// type:int
	// value:6
	// attr:I
	// type:ID
	// value:2

	Pattern f1 = Pattern.compile("numOfSelectedItems=(-?\\d+):int");
	Pattern f2 = Pattern.compile("numInShopCart=(-?\\d+):int");
	Pattern f3 = Pattern.compile("cartTotal=\"\\-?$(\\d+\\.\\d+)\":String");
	Pattern f4 = Pattern.compile("numInCompareCart=(-?\\d+):int");
	Pattern f5 = Pattern.compile("selectedProduct=(-?\\d+):int");

	public StateDescription parse(String event) {

		StateDescription state = new StateDescription();
		Matcher m1 = f1.matcher(event);

		if (m1.find()) {
			String v1 = m1.group(1);
			state.set("numOfSelectedItems", StateDescription.ValueType.INT, v1);
		}

		Matcher m2 = f2.matcher(event);

		if (m2.find()) {
			String v2 = m2.group(1);
			state.set("numInShopCart", StateDescription.ValueType.INT, v2);
		}

		Matcher m3 = f3.matcher(event);

		if (m3.find()) {
			String v3 = m3.group(1);
			state.set("cartTotal", StateDescription.ValueType.FLOAT, v3);
		}

		Matcher m4 = f4.matcher(event);

		if (m4.find()) {
			String v4 = m4.group(1);
			state.set("numInCompareCart", StateDescription.ValueType.INT, v4);
		}
		
		Matcher m5 = f5.matcher(event);

		if (m5.find()) {
			String v5 = m5.group(1);
			state.set("selectedProduct", StateDescription.ValueType.INT, v5);
		}

		return state;
	}

}
