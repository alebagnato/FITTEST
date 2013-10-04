package eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import java.util.Map;

/**
 * 
 * @author tiella
 */
public interface TemplateBasedGeneratorInterface {

	String generate(Model model, String packageStr, String templateFile, Map<String,Object> additionalData) throws FsmTesterException;
	String generate(Model model, String packageStr, String templateFile) throws FsmTesterException;
}
