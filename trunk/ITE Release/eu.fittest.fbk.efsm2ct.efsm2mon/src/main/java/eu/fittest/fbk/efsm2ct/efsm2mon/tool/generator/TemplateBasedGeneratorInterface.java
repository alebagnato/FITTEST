/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator;

import java.util.Map;

import eu.fittest.fbk.efsm2ct.efsm2mon.tool.FsmTesterException;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;

/**
 * 
 * @author tiella
 */
public interface TemplateBasedGeneratorInterface {

	String generate(Model model, String packageStr, String templateFile, Map<String,Object> additionalData) throws FsmTesterException;
	String generate(Model model, String packageStr, String templateFile) throws FsmTesterException;
}
