package eu.fittest.ucl.eventinf.individual;

import eu.fbk.se.fsm.FSM;

public interface Individual {
	public void defaultCrossover(Individual ind2);
	public void assignFitness(double fitnessValue);
	public double getFitness();
	public boolean isEvaluated();
	public Individual cloneIndividual(boolean keepFitnessValue);
	public String toString();
	public FSM getFSM();
	public void setFSM(FSM fsm);
}
