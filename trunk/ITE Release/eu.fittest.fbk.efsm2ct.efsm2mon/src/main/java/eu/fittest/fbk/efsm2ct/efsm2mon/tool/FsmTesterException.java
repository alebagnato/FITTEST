/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

/**
 * 
 * @author tiella
 */
public class FsmTesterException extends Exception {

	public FsmTesterException(String msg, Throwable ex) {
		super(msg, ex);
	}

	public FsmTesterException(String msg) {
		super(msg);
	}

}
