package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.io.File;
import java.util.List;
import java.util.Set;

import eu.fittest.fbk.efsm2ct.log2efsm.common.FlexstoreV3StateMappingRuleset;
import eu.fittest.fbk.efsm2ct.log2efsm.common.StateMapping;
import eu.fittest.fbk.efsm2ct.log2efsm.common.StateMappingRuleset;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Instance;

/**
 * Convert UU logs to FBK (model inference) logs
 * 
 * @author r. tiella
 * 
 */
public class ModelBasedConverter {

	private LogWriter logWriter = new LogWriter();

	private StateMappingRuleset rules = new FlexstoreV3StateMappingRuleset();

	public void convert(File fXmlFile, File outputFile, LogFilter filter) throws ConverterException {
		try {

			logWriter.startFile(outputFile);

			logWriter.writeLine("onLoad:__:load:__:[undef;__;]");

			LogReader logReader = new LogReader();
			List<Event> eventsIn = logReader.read(fXmlFile);
			
			List<Event> events = filter.filter(eventsIn);

			String outState = "[undef;__;]";

			for (Event e : events) {

				Instance recordEvent = e.getRecordEvent();

				Object targetIdVal = recordEvent.get("targetID").getValue();
				Object typeVal = recordEvent.get("type").getValue();

				String outEvent = String.format("%s:__:%s:__", targetIdVal, typeVal);

				Instance absState = e.getAppAbstractState();

				String newOutState = abstactState(absState);

				String line = String.format("%s:%s", outEvent, outState);

				outState = newOutState;

				logWriter.writeLine(line);
			}

			String line = String.format("%s:%s", "onunLoad:__:load:__", outState);
			logWriter.writeLine(line);

			logWriter.closeFile();

		} catch (Exception ex) {

			ex.printStackTrace();
			throw new ConverterException("cant'convert file:" + fXmlFile, ex);

		}

	}

	private String abstactState(Instance absState) throws ConverterException {

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

		if (!absState.getType().equals("AppAbstractState")) {
			throw new ConverterException("invalid object type:" + absState.getType());
		}

		Set<String> attrNames = absState.keySet();

		// ignored attributes are removed
		attrNames.remove("I");

		StringBuilder outBind = new StringBuilder();

		for (String a : attrNames) {

			TypedValue typedValue = absState.get(a);

			Object value = typedValue.getValue();
			String type = typedValue.getType();

			String outValue = null;
			String outType = null;

			StateMapping state = rules.map(a, type, value);

			if (state != null) {

				outValue = state.getOutSymbol();
				outType = state.getOutType();

			} else {

				System.err.println("unknown type " + type + " for attribute:" + a);

			}

			if (outValue != null && outType != null) {

				outBind.append(String.format("%s:%s=%s;__;", a, type, outValue));

			} else {
				System.err.println("WARNING: no coversion for " + a + " " + type + " " + value);
			}

		}

		String strState = String.format("[%s]", outBind);

		return strState;

	}

}
