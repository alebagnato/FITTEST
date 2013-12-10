package eu.fittest.ucl.eventinf.evolve;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.Vector;


import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.api.ModelInferenceListener;
import eu.fittest.ucl.eventinf.individual.Individual;
import eu.fittest.ucl.eventinf.individual.NSGA2Individual;
import eu.fittest.ucl.eventinf.reinsertion.ReinsertionStrategy;
import eu.fittest.ucl.eventinf.reinsertion.ReinsertionStrategy.Reinsertion;
import eu.fittest.ucl.eventinf.selection.Elitist;
import eu.fittest.ucl.eventinf.selection.Tournament;

public class NSGA2 extends GeneticAlgo implements Comparator<Individual> {

	public static enum FitnessObjective {unobs, unrec, size};
		
	public NSGA2(Random rand, ArrayList<ModelInferenceListener> listeners, 
			FSM[] basicFSM) {
		super(rand, listeners, basicFSM);
	}

	private double getObjectiveValueAsDouble(FitnessObjective obj, NSGA2Individual ind)
	{
		double value = 0;
		if(obj == FitnessObjective.size)
			value = ind.getFSM().size();
		else if(obj == FitnessObjective.unobs)
			value = unobservedStrings(ind.getFSM());
		else if(obj == FitnessObjective.unrec)
			value = unrecognizedTraces(ind.getFSM());
		else
			throw new RuntimeException("unrecognized fitness objective");
		
		return (value / (value + 1.0));
	}
	
	private ArrayList<NSGA2Individual> sortFrontier(final FitnessObjective obj, Set<NSGA2Individual> frontier)
	{
		ArrayList<NSGA2Individual> sorted = new ArrayList<NSGA2Individual>(frontier);
		Collections.sort(sorted,
				new Comparator<NSGA2Individual>()
				{

					public int compare(NSGA2Individual o1, NSGA2Individual o2) {
						FSM fsm1 = o1.getFSM();
						FSM fsm2 = o2.getFSM();
						if(obj == FitnessObjective.unobs)
						{
							Integer unObs1 = unobservedStrings(fsm1);
							Integer unObs2 = unobservedStrings(fsm2);
							return unObs1.compareTo(unObs2);
						}
						else if(obj == FitnessObjective.unrec)
						{
							Integer unRec1 = unobservedStrings(fsm1);
							Integer unRec2 = unobservedStrings(fsm2);
							return unRec1.compareTo(unRec2);
						}
						else if(obj == FitnessObjective.size)
							
						{
							Integer size1 = unobservedStrings(fsm1);
							Integer size2 = unobservedStrings(fsm2);
							return size1.compareTo(size2);
						}
						return 0;
					}
				});
		
		sorted.addAll(frontier);
		
		return sorted;
	}
	
	/*
	private void resetDominationInfoForCurrentPopulation()
	{
		for(Individual ind : population)
		{
			NSGA2Individual nsind = (NSGA2Individual)ind;
			nsind.resetMetrics();
		}
	}*/
	
	private ArrayList<Set<NSGA2Individual>> fastNondominatedSort(Individual[] population, 
			boolean saveFrontier)
	{
		ArrayList<Set<NSGA2Individual>> frontiers = new ArrayList<Set<NSGA2Individual>>();
		
		Set<NSGA2Individual> currentFrontier = new HashSet<NSGA2Individual>();
		
		//resetDominationInfoForCurrentPopulation();
		
		int i = 0;
		
		for(; i < population.length; ++i)
		{
			NSGA2Individual nsind = (NSGA2Individual)population[i];
			
			if(i == 0)
				nsind.resetMetrics();
			
			int j = i + 1;
			for(; j < population.length; ++j)
			{
				NSGA2Individual nsind2 = (NSGA2Individual)population[j];
				
				if(i == 0)
					nsind2.resetMetrics();
				
				FSM fsm1 = population[i].getFSM();
				FSM fsm2 = population[j].getFSM();
				//check if ind dominates ind2
				if(dominates(fsm1, fsm2))
				{
					nsind2.incrementDominatedCount();
					nsind.addDominatedIndividual(nsind2);
				}
				else if(dominates(fsm2, fsm1))
				{
					nsind.incrementDominatedCount();
					nsind2.addDominatedIndividual(nsind);
				}
			}
			
			if(nsind.getDominatedCount() == 0)
			{
				currentFrontier.add(nsind);
			}
		}
		
		if(saveFrontier)
			saveCurrentBestFrontier(currentFrontier);
		
		frontiers.add(currentFrontier);
		
		while(!currentFrontier.isEmpty())
		{
			Set<NSGA2Individual> newFrontier = new HashSet<NSGA2Individual>();
			Iterator<NSGA2Individual> it = currentFrontier.iterator();
			while(it.hasNext())
			{
				NSGA2Individual ind = (NSGA2Individual)it.next();
				Set<NSGA2Individual> dominated = ind.getDominatedInds();
				Iterator<NSGA2Individual> it2 = dominated.iterator();
				while(it2.hasNext())
				{
					NSGA2Individual ind2 = (NSGA2Individual)it2.next();
										
					if(ind2.decrementDominatedCount() == 0)
					{
						newFrontier.add(ind2);
					}
				}
			}
			
			if(newFrontier.size() > 0)
				frontiers.add(newFrontier);
			
			currentFrontier = newFrontier;
		}
		
		return frontiers;
	}
	
	private void assignCrowdingDistances(Set<NSGA2Individual> frontier)
	{
		if(frontier.isEmpty()) return;
		
		//initialize distances to 0
		Iterator<NSGA2Individual> it = frontier.iterator();
		while(it.hasNext())
		{
			NSGA2Individual ind = (NSGA2Individual)it.next();
			ind.setCrowdingDistance(0);
		}
		for (FitnessObjective obj : FitnessObjective.values())
		{
			if(!super.useMinSize && obj == FitnessObjective.size) continue;
			
			ArrayList<NSGA2Individual> sorted = sortFrontier(obj, frontier);
			sorted.get(0).setCrowdingDistance(Double.MAX_VALUE);
			sorted.get(sorted.size() - 1).setCrowdingDistance(Double.MAX_VALUE);
			for(int i = 1; i < sorted.size() - 2; ++i)
			{
				double distance = sorted.get(i).getCrowdingDistance() + 
				(getObjectiveValueAsDouble(obj, sorted.get(i + 1)) - 
						getObjectiveValueAsDouble(obj, sorted.get(i - 1)));
				
				sorted.get(i).setCrowdingDistance(distance);
			}
		}
	}
	
	private int compareRank(int i1, int i2)
	{
		if(i1 < i2)
			return -1;
		else if(i1 > i2)
			return +1;
		else
			return 0;
	}
	
	private int compareDistance(double d1, double d2)
	{
		if(d1 > d2)
			return -1;
		else if(d1 < d2)
			return +1;
		else
			return 0;
	}
	
	private void saveCurrentBestFrontier(Set<NSGA2Individual> frontier)
	{
		if(frontier.isEmpty()) return;
		
		Iterator<NSGA2Individual> it = frontier.iterator();
		globallyNonDominated.clear();
		
		while(it.hasNext())
		{
			NSGA2Individual ind = (NSGA2Individual)it.next();
			globallyNonDominated.add(ind.getFSM());
		}
		
		fixGloballyNonDominated();
	}
	
	@Override
	void initializePopulation()
	{
		muOp = new MutationOperator(super.random, super.basicFSM, super.traces,
				super.mutationProbability, super.maxAddTraceAttempts,
				super.maxFSMSize, super.probRandomMerge);

		crOp = new CrossoverOperator(super.random, super.offspringPopulationSize, false,
				super.crossoverProbability, super.maxFSMSize);

		reinsertionOp = new ReinsertionStrategy(super.random, Reinsertion.elitist);

		population = new NSGA2Individual[super.populationSize];
		
	    selectionOp = new Tournament(super.random, super.offspringPopulationSize);
	    
		for (int i = 0 ; i < super.populationSize ; i++) {
			int j = super.random.nextInt(basicFSM.length);
			population[i] = new NSGA2Individual(i,
					basicFSM[j].clone(""));
		}
	}
	
	@Override
	public int compare(Individual arg0, Individual arg1) {
		int o1Dominated, o2Dominated;
		NSGA2Individual ind1 = (NSGA2Individual)arg0;
		NSGA2Individual ind2 = (NSGA2Individual)arg1;
		
		o1Dominated = ind1.getDominatedCount();
		o2Dominated = ind2.getDominatedCount();
		
		if(o1Dominated == o2Dominated)
		{
			return compareDistance(ind1.getCrowdingDistance(),
					ind2.getCrowdingDistance());
		}
		else 
		{
			//prefer low ranked solutions, i.e. with fewer 
			//dominating solutions
			return compareRank(o1Dominated, o2Dominated);
		}
	}
	/*
	private void printFrontiers(ArrayList<Set<NSGA2Individual>> frontiers)
	{
		for(Set<NSGA2Individual> current : frontiers)
		{
			Iterator<NSGA2Individual> it = current.iterator();
			boolean printedFrontierNumber = false;
			while(it.hasNext())
			{
				NSGA2Individual ind = (NSGA2Individual)it.next();
				if(!printedFrontierNumber)
				{
					System.out.println("Front:" + ind.getDominatedCount());
					System.out.println("========");
					printedFrontierNumber = true;
				}
				System.out.println("UnObs = " + unobservedStrings(ind.getFSM()));
				System.out.println("UnRec = " + unrecognizedTraces(ind.getFSM()));
				System.out.println("Size  = " + ind.getFSM().size());
				System.out.println("--------");
			}
			
			System.out.println("========");
		}
	}
	*/
	private ArrayList<NSGA2Individual> convertFrontiersToArrayList(ArrayList<Set<NSGA2Individual>> frontiers)
	{
		ArrayList<NSGA2Individual> ranked = new ArrayList<NSGA2Individual>();
		
		int numberOfFrontiers = frontiers.size();
		
		for(int i = 0; i < numberOfFrontiers; ++i)
		{
			Set<NSGA2Individual> frontier = frontiers.get(i);

			Iterator<NSGA2Individual> it = frontier.iterator();
			
			while(it.hasNext())
			{
				ranked.add((NSGA2Individual)it.next());
			}
		}
		
		return ranked;
	}
	
	@Override
	public void run()
	{
		//create a random population
		initializePopulation(); 
						
		//sort population into frontiers, where 1st frontier only has nondominated, 
		//2nd frontier has solutions dominated by 1 other individual etc
		ArrayList<Set<NSGA2Individual>> frontiers = fastNondominatedSort(population, false);
		
		//convert frontiers into a ranked population. All individuals in the 1st frontier are ranked 
		//top etc. The order within a frontier is random
		ArrayList<NSGA2Individual> rankedPopulation = convertFrontiersToArrayList(frontiers);
		
		for(int i = 0; i < rankedPopulation.size(); ++i)
		{
			population[i] = rankedPopulation.get(i);
		}
		
		//create mating population. tournament selection is used. The number of tournaments
		//is equal to the OFFSPRING_POPULATION_SIZE because each tournament selects one individual
		ArrayList<Individual> selectedInd = this.selectionOp.getParents(population, this);
		
		//do crossover
	    ArrayList<Individual> offspringPopulation = this.crOp.crossover(selectedInd);
	    
		//do mutation
		this.muOp.mutate(offspringPopulation);
				
		//now do the next generations
		for (int k = 1 ; k < super.generations ; ++k) {
			
			super.reportProgress((k * 100) / super.generations, null);
			
			Individual[] combinedPopulation = new Individual[population.length + offspringPopulation.size()];
			
			//combine current population and offspring population
			//System.arraycopy(population, 0, combinedPopulation, 0, population.length);
			for(int i = 0; i < population.length; ++i)
			{
				combinedPopulation[i] = population[i];
			}
			
			for(int i = 0; (i + population.length) < combinedPopulation.length && 
							i < offspringPopulation.size(); ++i)
			{
				combinedPopulation[population.length + i] = offspringPopulation.get(i);
			}
			
			//create non dominated frontiers
			frontiers = fastNondominatedSort(combinedPopulation, false);
			
			//until population is filled..
			int numberOfFrontiers = frontiers.size();
			int popIndex = 0;
			
			for(int i = 0; i < numberOfFrontiers; ++i)
			{
				Set<NSGA2Individual> frontier = frontiers.get(i);
								
				assignCrowdingDistances(frontier);
				
				//check if frontier is the *last* frontier to be added
				int capacity = super.populationSize - popIndex;
				if(frontier.size() > capacity)
				{
					//convert set to list
					ArrayList<NSGA2Individual> tosort = new ArrayList<NSGA2Individual>();
					tosort.addAll(frontier);
					
					//sort according to crowding distance (note that rank will be the same)
					Collections.sort(tosort, this);
					
					//fill up population
					int ii = 0;
					
					while(popIndex < super.populationSize)
					{
						population[popIndex++] = tosort.get(ii++);
					}
					
					break;
				} 
				else
				{
					Iterator<NSGA2Individual> it = frontier.iterator();
					while(it.hasNext())
					{
						population[popIndex++] = (NSGA2Individual)it.next();
					}
				}
				
			}
			
			//select mating population
			selectedInd = this.selectionOp.getParents(population, this);
			
			//do crossover
		    offspringPopulation = this.crOp.crossover(selectedInd);
		    
			//do mutation
			this.muOp.mutate(offspringPopulation);

		}
	}

	@Override
	public void saveNonDominatedToFile(File folder) {
		//make sure we get the current pareto front by creating frontiers.
		//This will update globallyNonDominated vector
		fastNondominatedSort(population, true);
		
		super.saveNonDominatedToFile(folder);
	}
	
	@Override
	public Vector<FSM> getParetoFront() {
		fastNondominatedSort(population, true);
		return this.globallyNonDominated;
	}
}
