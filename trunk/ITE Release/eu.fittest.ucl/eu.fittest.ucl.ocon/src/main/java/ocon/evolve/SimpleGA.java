package ocon.evolve;

import java.util.ArrayList;
import java.util.Random;

import ocon.entry.BestIndividualSetter;
import ocon.evolve.selection.SelectionStrategy;
import ocon.fitness.FitnessFunction;
import ocon.individual.Individual;
import ocon.individual.KingPropertyIndividual;
import ocon.search.SearchMethod;
import eu.fittest.component.optimizer.IOptimizerRequiredServices;

public class SimpleGA extends SearchMethod {
	
	private ArrayList<Individual> population;
	private final int populationSize;
	private final SelectionStrategy parentSelector;
	
	private int currentGeneration;	
	private final int maxGenerations;
	
	public SimpleGA(IOptimizerRequiredServices requiredServices, Random rnd, FitnessFunction ff, BestIndividualSetter iBestIndSetter,
			int populationSize, int maxGenerations, SelectionStrategy selector)
	{
		super(requiredServices, rnd, ff, iBestIndSetter);
		this.maxGenerations = maxGenerations;
		this.populationSize = populationSize;
		this.population = new ArrayList<Individual>(populationSize);
		this.parentSelector = selector;
		
		this.currentGeneration = 0;
	}
	
	private void evaluatePopulation(ArrayList<Individual> pop)
	{
		int current = 0;
		if(DEBUG){
			System.out.println("Population size: "+pop.size());
		}
		while(current < pop.size() && !super.terminate)
		{
			if(DEBUG){
				System.out.println("Population number: "+current);
			}
			Individual ind = pop.get(current);
			if(!ind.isEvaluated())
			{
				super.fitnessFunction.evaluate(ind);
				
				if(super.bestIndividual == null || 
						super.fitnessFunction.compare(ind, super.bestIndividual) < 0)
				{
					super.bestIndividual = ind.cloneIndividual(true);
					super.iBestIndSetter.setBestIndividual(super.bestIndividual);
					
					if(super.bestIndividual.isIdeal())
					{
						super.terminate = true;
						break;
					}
				}
			}
			current += 1;
		}
	}
	
	private void generateTwoIndices(int[] out_indices, int limit)
	{
		int _i1 = super.random.nextInt(limit);
		int _i2 = super.random.nextInt(limit);
		while(_i1 == _i2 && limit > 1)
		{
			_i2 = super.random.nextInt(limit);
		}
		out_indices[0] = _i1;
		out_indices[1] = _i2;
	}
	private ArrayList<Individual> crossover(ArrayList<Individual> matingPop)
	{
		int size = matingPop.size();
		int mates = size / 2;
		int[] indices = new int[2];
		int cnt = 0;
		ArrayList<Individual> offspring = new ArrayList<Individual>(size);
		while(cnt < mates)
		{
			generateTwoIndices(indices, size);
			Individual p1 = matingPop.get(indices[0]);
			Individual p2 = matingPop.get(indices[1]);
			Individual o1 = p1.cloneIndividual(false);
			Individual o2 = p2.cloneIndividual(false);
			o1.defaultCrossover(o2);
			offspring.add(o1);
			offspring.add(o2);
			cnt += 1;
		}
		
		return offspring;
	}
	private void mutate(ArrayList<Individual> offspring)
	{
		int size = offspring.size();
		int cnt = 0;
		while(cnt < size)
		{
			offspring.get(cnt).defaultMutate();
			cnt += 1;
		}
	}
	private void updatePopulation(ArrayList<Individual> offspring)
	{
		for(Individual ind : offspring)
		{
			for(int i = 0; i < this.population.size(); ++i)
			{
				Individual parent = this.population.get(i);
				if(super.fitnessFunction.compare(ind, parent) < 0)
				{
					this.population.set(i, ind);		
				}
			}
		}
	}
	
	private void initializePopulation()
	{
		for(int i = 0; i < this.populationSize; ++i)
		{
			this.population.add(i, new KingPropertyIndividual(super.random));
		}
	}
	
	public void run()
	{
		initializePopulation();
		
		if(DEBUG)
			System.out.println("Generation: " + this.currentGeneration);
		
		evaluatePopulation(this.population); //evaluate initial population
		
		while(!super.terminate && 
				this.currentGeneration < this.maxGenerations)
		{
			if(DEBUG)
				System.out.println("Generation: " + this.currentGeneration);
			ArrayList<Individual> mating = this.parentSelector.getParents(this.population, 
					super.fitnessFunction);
			if(super.terminate) break;
			ArrayList<Individual> offspring = crossover(mating);
			if(super.terminate) break;
			mutate(offspring);
			if(super.terminate) break;
			evaluatePopulation(offspring);
			if(super.terminate) break;
			updatePopulation(offspring);
			if(super.terminate) break;
			this.currentGeneration++;
			super.setProgress(((this.currentGeneration * (100-1)) / this.maxGenerations));//keep one percent for saving configuration
		}
	}
}
