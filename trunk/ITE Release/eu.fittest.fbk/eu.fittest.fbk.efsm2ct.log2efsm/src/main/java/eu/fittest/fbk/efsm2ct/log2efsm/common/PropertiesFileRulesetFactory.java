package eu.fittest.fbk.efsm2ct.log2efsm.common;

import java.io.File;
import java.io.FileReader;
import java.util.Properties;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.ConverterException;


public class PropertiesFileRulesetFactory {

	public static StateMappingRuleset loadFromFile(File f) throws ConverterException {

		try {

			StateMappingRuleset rs = new StateMappingRuleset();

			Properties p = new Properties();

			p.load(new FileReader(f));

			int len = Integer.parseInt(p.getProperty("rule.length"));

			for (int i = 1; i <= len; i++) {

				
				String attrNameRegex = p.getProperty(String.format("rule.%s.attrNameRegex", i));
				String inType = p.getProperty(String.format("rule.%s.inType", i));
				String inPred = p.getProperty(String.format("rule.%s.inPred", i));
				String outType = p.getProperty(String.format("rule.%s.outType", i));
				String outSymbol = p.getProperty(String.format("rule.%s.outSymbol", i));
				String outPredFmt = p.getProperty(String.format("rule.%s.outPredFmt", i));

				StateMappingRule rule = new StateMappingRule();				

				rule.setAttrNameRegex(attrNameRegex);
				rule.setInType(inType);
				rule.setInPred(inPred);
				rule.setOutType(outType);
				rule.setOutSymbol(outSymbol);
				rule.setOutPredFmt(outPredFmt);

				rs.addRule(rule);
				
			}

			return rs;

		} catch (Exception ex) {
			throw new ConverterException("Can't load ruleset from:" + f, ex);
		}

	}

}
