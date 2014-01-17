/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.evolve;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import eu.fittest.ucl.autoabs.cluster.Cluster;
import eu.fittest.ucl.autoabs.cluster.ClusterAlgo;
import eu.fittest.ucl.autoabs.utils.Utils;
import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

import eu.fbk.se.fsm.FSM;

/**
 * Class that represents an individual to optimize. The genes denote a mapping from
 * concrete states to the cluster that state belongs to
 */
public class Chromosome {
	
	private final Random rnd;
	private final ClusterAlgo clusterAlgo;
	
	private final int k;
	
	//a list of genes
	//each element represents a mapping from unique concrete state to a cluster
	private ArrayList<Gene> genes;
	
	private double mutationProbability;
	
	private int nonDeterministicTransition;
	private int fsmSize;
	private int numInfeasibleSequences;
	
	private double fitness;
	private boolean isEvaluated;
	
	private FSM fsm;
	
	public Chromosome(Random rnd, ClusterAlgo clusterAlgo, LinkedHashSet<ConcreteState> states, double mutationProbability)
	{
		//exclude k == 1 and k == maxK, because these denote either
		//singleton clusters, or a single cluster containing all states
		//nextInt = [0,maxK[, therefore scale to [1,maxK]
		//scale to [2, maxK-1]
		this(rnd, clusterAlgo, states, rnd.nextInt(clusterAlgo.getMaxK()) + 1, mutationProbability);
	}
	
	public Chromosome(Random rnd, ClusterAlgo clusterAlgo, LinkedHashSet<ConcreteState> states, int k, double mutationProbability){
		this.rnd = rnd;
		this.clusterAlgo = clusterAlgo;
		this.genes = new ArrayList<Gene>(states.size());
		this.mutationProbability = (mutationProbability == -1)? (1.0 / (double)this.genes.size()):mutationProbability;
		this.fitness = Double.MAX_VALUE;
		this.numInfeasibleSequences = this.nonDeterministicTransition = this.fsmSize = Integer.MAX_VALUE;
		this.isEvaluated = false;
		this.k = k;
		this.fsm = null;
		for(ConcreteState state : states) {
			this.genes.add(new Gene(state, this.clusterAlgo.getClusterId(state, this.k)));
		}
	}
	
	private Chromosome(Chromosome seed)
	{
		this.rnd = seed.rnd;
		this.clusterAlgo = seed.clusterAlgo;
		this.genes = new ArrayList<Gene>(seed.genes.size());		
		this.mutationProbability = seed.mutationProbability;
		this.fitness = seed.fitness;
		this.nonDeterministicTransition = seed.nonDeterministicTransition;
		this.numInfeasibleSequences = seed.numInfeasibleSequences;
		this.fsmSize = seed.fsmSize;
		this.isEvaluated = seed.isEvaluated;
		this.k = seed.k;
		for(int i = 0; i < seed.genes.size(); i++)
		{
			this.genes.add(new Gene(seed.genes.get(i)));
		}
		if(seed.fsm != null){
			this.fsm = seed.fsm.clone("");
		} else {
			this.fsm = null;
		}
	}
	
	/* START PROPERTIES */
	public void setFSM(FSM fsm) {
		this.isEvaluated = true;
		this.fsm = fsm;
	}
	public FSM getFSM() {
		return this.fsm;
	}
	
	public int getK() {
		return this.k;
	}
	
	public void setMutationProbability(double prob)
	{
		this.mutationProbability = prob;
	}
	public double getMutationProbability()
	{
		return this.mutationProbability;
	}
	public void setFitness(double fitnessValue)
	{
		this.fitness = fitnessValue;
	}
	public double getFitness()
	{
		return this.fitness;
	}
	public void setNDET(int ndet) {
		this.nonDeterministicTransition = ndet;
	}
	public void setInfeasible(int inf) {
		this.numInfeasibleSequences = inf;
	}
	public void setFSMSize(int size) {
		this.fsmSize = size;
	}
	public int getNDET() {
		return this.nonDeterministicTransition;
	}
	public int getInfeasible() {
		return this.numInfeasibleSequences;
	}
	public int getFSMSize() {
		return this.fsmSize;
	}
	public Integer getClusterId(ConcreteState s) {
		Gene g;
		for(int i = 0; i < this.genes.size(); i++){
			g = this.genes.get(i);
			if(g.getConcreteState().equals(s))
				return g.getClusterId();
		}
		return null;
	}
	/* END PROPERTIES */
	
	/** A 1 point crossover to swap cluster ids
	 * @param ind2 The chromosome to x-over with
	 */
	public void defaultCrossover(Chromosome ind2)
	{
		int limit = this.genes.size();
		int xpoint = this.rnd.nextInt(limit);
		for(int i = xpoint; i < limit; i++)
		{
			Gene tmp = this.genes.get(i); 
			this.genes.set(i, ind2.genes.get(i));
			ind2.genes.set(i, tmp);
		}
	}

	/** This operator either:
	 *  - merges the two closest clusters
		- picks a random cluster and splits it
		- moves one concrete state from one cluster into another
	 */
	public void mutate(int muOp) {
		Collection<Cluster> clusters = this.generateClusters();
		boolean done = false;
		if(muOp == 0 && clusters.size() > 2) {
			//merge
			Cluster[] closest = this.clusterAlgo.getClosestClusters(clusters);
			int refId = closest[0].getID();
			int id = closest[1].getID();
			Gene g;
			for(int i = 0; i < this.genes.size(); i++) {
				g = this.genes.get(i);
				if(g.getClusterId() == id) {
					g.setClusterId(refId);
				}
			}
			done = true;
		} else if(muOp == 1) {
			//split cluster
			int tries = 0;
			int maxTries = clusters.size();
			Set<Integer> usedIds = new HashSet<Integer>();
			outer:
			while(tries < maxTries) {
				//pick a random cluster - slow :(
				int index;
				do {
					index = this.rnd.nextInt(maxTries); //i.e. clusters.size()
				} while ( usedIds.contains(index) );
				usedIds.add(index);
				int count = 0;
				for(Cluster c : clusters) {
					if(count == index) {
						int numStates = c.getNumStates();
						if(numStates > 1) {
							Cluster c1 = new Cluster();
							Cluster c2 = new Cluster();
							int cid = c.getID();
							int id1 = c1.getID();
							int id2 = c2.getID();
							int splitPoint = numStates == 2? 1:this.rnd.nextInt(numStates - 1) + 1;
							int numSplit = 0;
							//now update all the genes
							for(Gene g : this.genes) {
								if(g.getClusterId() == cid) {
									if(numSplit < splitPoint)
										g.setClusterId(id1);
									else
										g.setClusterId(id2);
									numSplit++;
								}
							}
							c1 = c2 = null;
							done = true;
							break outer;
						} else 
							break; //continue with while loop so I pick a new cluster
					}
					count++;
				}
				tries++;
			}
		}
		if(!done) {
			//move state
			int srcIndex = this.rnd.nextInt(this.genes.size());
			if(clusters.size() == 1) {
				Cluster c = new Cluster();
				this.genes.get(srcIndex).setClusterId(c.getID());
			} else {
				int targetIndex = this.rnd.nextInt(this.genes.size());
				int srcId = this.genes.get(srcIndex).getClusterId();
				int targetId = this.genes.get(targetIndex).getClusterId();
				while(srcId == targetId) {
					targetIndex = this.rnd.nextInt(this.genes.size());
					targetId = this.genes.get(targetIndex).getClusterId();
				}
				this.genes.get(srcIndex).setClusterId(targetId);
			}
		}
	}
	
	/** This function generates clusters for all the trace gene lists in the 
	 * chromosome
	 * @return A collection of clusters
	 */
	public Collection<Cluster> generateClusters() {
		Map<Integer, Cluster> mapping = new HashMap<Integer, Cluster>();
		Gene g = null;
		Cluster cluster = null;
		for(int i = 0; i < this.genes.size(); i++) {
			g = this.genes.get(i);
			int id = g.getClusterId();
			cluster = mapping.get(id);
			if(cluster != null) {
				cluster.addState(g.getConcreteState());
			} else {
				cluster = new Cluster(id);
				cluster.addState(g.getConcreteState());
				mapping.put(id, cluster);
			}
		}
		return mapping.values();
	}
	
	//only package level access for testing
	ArrayList<Gene> getGenes() {
		return this.genes;
	}
	
	public void defaultMutate()
	{
		//randomly change the cluster an invariant belongs to
		if(this.rnd.nextDouble() < this.mutationProbability)
		{
			int muOp = this.rnd.nextInt(3);
			this.mutate(muOp);
		}
	}
	
	public boolean isIdeal()
	{
		return false;
	}
	public boolean isEvaluated()
	{
		return this.isEvaluated;
	}
	public Chromosome cloneChromosome(boolean keepFitnessValue)
	{
		Chromosome clone = new Chromosome(this);
		if(!keepFitnessValue) {
			clone.fitness = Double.MAX_VALUE;
			clone.nonDeterministicTransition = clone.numInfeasibleSequences = clone.fsmSize = Integer.MAX_VALUE;
			clone.isEvaluated = false;
		} 
		return clone;
	}
	
	@Override
	public String toString()
	{
		Collection<Cluster> clusters = this.generateClusters();
		StringBuilder sb = new StringBuilder();
		for(Cluster c : clusters)
		{
			sb.append("Cluster " + c.getID() + ":" + Utils.eol);
			for(ConcreteState state : c.getStates())
			{
				sb.append(state.toString() + Utils.eol);
			}
			sb.append(Utils.eol);
		}
		return sb.toString();
	}
	
	/**
	 * This function groups states together that satisfy the same invariants
	 * @param clusterIds The new 'cluster' id for the states.
	 */
	public void mergeStates(Set<Integer> clusterIds) {
		if(clusterIds.size() > 1) {
			Iterator<Integer> it = clusterIds.iterator();
			int refId = it.next();
			//update cluster ids
			Gene g;
			for(int i = 0; i < this.genes.size(); i++){
				g = this.genes.get(i);
				int id = g.getClusterId();
				if(id != refId && clusterIds.contains(id)) {
					g.setClusterId(refId);
				}
			}
		}
	}
	/** Help clear up resources for that chromosome? */
	public void dispose() {
		this.genes.clear();
		this.genes = null;
		if(this.fsm != null){
			this.fsm.dispose();
			this.fsm = null;
		}
	}
}
