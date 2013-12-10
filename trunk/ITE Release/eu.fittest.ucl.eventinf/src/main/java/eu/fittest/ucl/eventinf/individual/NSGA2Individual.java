package eu.fittest.ucl.eventinf.individual;

import java.util.HashSet;
import java.util.Set;

import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.eventinf.individual.Individual;

public class NSGA2Individual implements Individual {

	private FSM fsm;
	private double fitness;
	private boolean evaluated;
	
	private final int id;
	
	private int numIndsDominatedBy;
	private double crowdingDistance;
	private Set<NSGA2Individual> dominatedInds;
	
	public NSGA2Individual(int id, FSM basicFSM)
	{
		fsm = basicFSM;
		fitness = 0;
		evaluated = false;
		
		this.id = id;
		this.numIndsDominatedBy = 0;
		this.crowdingDistance = 0;
		this.dominatedInds = new HashSet<NSGA2Individual>();
	}
	public NSGA2Individual(NSGA2Individual ind)
	{
		this.fsm = ind.getFSM();
		this.fitness = ind.getFitness();
		this.evaluated = ind.isEvaluated();
		this.id = ind.getId();
		this.numIndsDominatedBy = ind.getDominatedCount();
		this.crowdingDistance = ind.getCrowdingDistance();
		this.dominatedInds = ind.getDominatedInds();
	}
	public NSGA2Individual(NSGA2Individual ind, boolean copyFitness)
	{
		this.fsm = ind.getFSM();
		this.fitness = copyFitness? ind.getFitness():0;
		this.evaluated = ind.isEvaluated();
		this.id = ind.getId();
		this.numIndsDominatedBy = ind.getDominatedCount();
		this.crowdingDistance = ind.getCrowdingDistance();
		this.dominatedInds = ind.getDominatedInds();
	}

	public void assignFitness(double fitnessValue) {
		// TODO Auto-generated method stub
		fitness = fitnessValue;
		evaluated = true;
	}


	public Individual cloneIndividual(boolean keepFitnessValue) {
		// TODO Auto-generated method stub
		return new NSGA2Individual(this, keepFitnessValue);
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

	public int getId()
	{
		return this.id;
	}
	public int getDominatedCount()
	{
		return this.numIndsDominatedBy;
	}
	public Set<NSGA2Individual> getDominatedInds()
	{
		return this.dominatedInds;
	}
	public void setCrowdingDistance(double d)
	
	{
		this.crowdingDistance = d;
	}
	public double getCrowdingDistance()
	{
		return this.crowdingDistance;
	}
	public void addDominatedIndividual(NSGA2Individual ind)
	{
		this.dominatedInds.add(ind);
	}
	public void incrementDominatedCount()
	{
		++this.numIndsDominatedBy;
	}
	public int decrementDominatedCount()
	{
		--this.numIndsDominatedBy;
		return this.numIndsDominatedBy;
	}

	public void resetMetrics()
	{
		this.numIndsDominatedBy = 0;
		this.dominatedInds.clear();
	}
}
