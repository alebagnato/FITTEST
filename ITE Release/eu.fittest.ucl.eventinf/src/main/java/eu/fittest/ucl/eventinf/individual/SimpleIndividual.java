package eu.fittest.ucl.eventinf.individual;

import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.eventinf.individual.Individual;

public class SimpleIndividual implements Individual {

	private FSM fsm;
	private double fitness;
	private boolean evaluated;
	
	public SimpleIndividual(FSM basicFSM)
	{
		fsm = basicFSM;
		fitness = 1;
		evaluated = false;
	}
	public SimpleIndividual(FSM fsm, double fitness, boolean evaluated)
	{
		this.fsm = fsm;
		this.fitness = fitness;
		this.evaluated = evaluated;
	}

	public void assignFitness(double fitnessValue) {
		// TODO Auto-generated method stub
		fitness = fitnessValue;
		evaluated = true;
	}


	public Individual cloneIndividual(boolean keepFitnessValue) {
		// TODO Auto-generated method stub
		if(keepFitnessValue)
			return new SimpleIndividual(fsm.clone(""), fitness, evaluated);
		else
			return new SimpleIndividual(fsm.clone(""));
	}


	public void defaultCrossover(Individual ind2) {
		// TODO Auto-generated method stub

	}


	public double getFitness() {
		// TODO Auto-generated method stub
		return fitness;
	}


	public boolean isEvaluated() {
		// TODO Auto-generated method stub
		return evaluated;
	}


	public FSM getFSM() {
		// TODO Auto-generated method stub
		return fsm;
	}


	public void setFSM(FSM fsm) {
		// TODO Auto-generated method stub
		this.fsm = fsm;
	}

}
