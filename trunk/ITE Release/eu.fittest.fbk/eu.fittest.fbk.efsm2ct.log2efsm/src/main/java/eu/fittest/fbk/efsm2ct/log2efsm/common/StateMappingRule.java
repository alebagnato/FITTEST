package eu.fittest.fbk.efsm2ct.log2efsm.common;

import java.io.StringReader;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import bsh.EvalError;
import bsh.Interpreter;

/**
 * models a rule to map an observed state, i.e. as listed into UU logs, to an
 * abstract state.
 * 
 * attrNameRegex, inType, expr -> outType,symValue,predicateFmt
 * 
 * e.g.
 * 
 * '.*','int',' $1 == 0' -> 'int','Zero','$1 == 0' 
 * '.*','Array','$1.size() > 0$-> 'int','GreaterThanZero','$1 > 0'
 * 
 * @author tiella
 * 
 */

/*
 * // typical instance // // instance type:AppAbstractState //
 * attr:catalogContents // type:Array //
 * value:eu.fittest.modelInference.logConverter.model.Instance@6345e044 //
 * attr:cartTotal // type:String // value:"$0.00" // attr:numInShopCart //
 * type:int // value:0 // attr:compareCartContents // type:Array //
 * value:eu.fittest.modelInference.logConverter.model.Instance@86c347 //
 * attr:shoppingCartContents // type:Array //
 * value:eu.fittest.modelInference.logConverter.model.Instance@f7e6a96 //
 * attr:numInCompareCart // type:int // value:0 // attr:numOfSelectedItems //
 * type:int // value:6 // attr:I // type:ID // value:2
 */

public class StateMappingRule {

	private String attrNameRegex;

	private String inType;
	private String inPred;

	private String outType;
	private String outSymbol;
	private String outPredFmt;

	public String getAttrNameRegex() {
		return attrNameRegex;
	}

	public void setAttrNameRegex(String attrNameRegex) {
		this.attrNameRegex = attrNameRegex;
	}

	public String getInType() {
		return inType;
	}

	public void setInType(String inType) {
		this.inType = inType;
	}

	public String getInPred() {
		return inPred;
	}

	public void setInPred(String inPred) {
		this.inPred = inPred;
	}

	public String getOutType() {
		return outType;
	}

	public void setOutType(String outType) {
		this.outType = outType;
	}

	public String getOutSymbol() {
		return outSymbol;
	}

	public void setOutSymbol(String outSymbol) {
		this.outSymbol = outSymbol;
	}

	public String getOutPredFmt() {
		return outPredFmt;
	}

	public void setOutPredFmt(String outPredFmt) {
		this.outPredFmt = outPredFmt;
	}

	public boolean match(String attrName, String inType, Object value) {

		boolean res = false;

		if (attrNameRegex == null || Pattern.matches(attrNameRegex, attrName)) {

			if (this.inType == null || this.inType.equals(inType)) {

				if (this.inPred != null) {

					try {
						res = eval(value);
					} catch (EvalError e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				} else {

					res = true;

				}

			}

		}

		return res;

	}

	private boolean eval(Object value) throws EvalError {

		Interpreter i = new Interpreter(); // Construct an interpreter

		i.set("v", value);

		Boolean res;

		try {
			res = (Boolean) (i.eval(new StringReader(this.inPred + ";")));
		} catch (EvalError e) {

			Logger.getAnonymousLogger().warning("can't evaluate:" + this.inPred + " on: " + value + " (" + value.getClass() + ")");
			throw e;

		}

		return res;

	}

}
