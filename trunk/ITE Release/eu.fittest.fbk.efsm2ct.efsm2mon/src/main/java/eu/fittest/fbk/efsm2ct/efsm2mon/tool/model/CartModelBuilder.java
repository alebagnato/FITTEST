/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.model;

/**
 * 
 * @author tiella
 */
public class CartModelBuilder {

	public static Model build() {

		State q0 = new State();
		q0.setInitial(true);
		q0.setPredicate("n == 0");
		q0.setId(0);
		q0.setName("s" + q0.getId());

		State q1 = new State();
		q1.setPredicate("n == 1");
		q1.setId(1);
		q1.setName("s" + q1.getId());

		State q2 = new State();
		q2.setPredicate("n >= 1");
		q2.setId(2);
		q2.setName("s" + q2.getId());

		Transition t1 = new Transition();
		t1.setSource(q0);
		t1.setTarget(q1);

		Transition t3 = new Transition();
		t3.setSource(q1);
		t3.setTarget(q2);

		Mutator add = new Mutator();
		add.setName("add");
		add.addArgumentType("int");
		add.addTransition(t1);
		add.addTransition(t3);

		Mutator rem = new Mutator();
		rem.setName("rem");
		rem.addArgumentType("int");

		Inspector n = new Inspector();
		n.setName("n");
		n.setType("int");

		Model m = new Model();

		m.setSutName("Cart");

		m.setSutPackage("adabutest");

		m.addState(q0);
		m.addState(q1);
		m.addState(q2);
		m.setInitialState(q0);

		m.addTransition(t1);
		m.addTransition(t3);

		m.addMutator(add);

		m.addInspector(n);

		return m;

	}

}
