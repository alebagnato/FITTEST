package eu.fittest.ucl.eventinf.reinsertion;

import java.util.*;

import eu.fittest.ucl.eventinf.individual.Individual;

public class ReinsertionStrategy {
	public enum Reinsertion {random, elitist, top}
	
	private final Random ran;
	private Reinsertion strategy;
	
	public int topPercent = 0;
	
	public ReinsertionStrategy(Random ran)
	{
		this.ran = ran;
		strategy = Reinsertion.elitist;
	}
	public ReinsertionStrategy(Random ran, Reinsertion strategy)
	{
		this.ran = ran;
		this.strategy = strategy;
	}
	
	public  Individual[] updatePopulation(Individual[] parent,
			ArrayList<Individual> offspring, Comparator<Individual> comparer)
	{
		Individual[] newPopulation = new Individual[parent.length];
		int i = 0;
		if(strategy == Reinsertion.top)
		{
			int slice = (int) Math.floor((offspring.size() * topPercent) / 100);
			for(; i < slice; ++i)
			{
				newPopulation[i] = offspring.get(i).cloneIndividual(true);
			}
			for(; i < parent.length; ++i)
			{
				newPopulation[i] = parent[i];
			}
		}
		else if(strategy == Reinsertion.random)
		{
			int parentIndex = 0;
			int offspringIndex = 0;
			for(; i < parent.length; ++i)
			{
				if(ran.nextInt() % 2 == 0
						&& offspringIndex < offspring.size())
				{
					newPopulation[i] = offspring.get(offspringIndex++).cloneIndividual(true);
				}
				else
				{
					newPopulation[i] = parent[parentIndex++];
				}
			}
		}
		else if(strategy == Reinsertion.elitist)
		{
			Set<Integer> removedIndeces = new HashSet<Integer>();
			
			for(Individual ind : offspring)
			{
				for(int j = 0; j < parent.length; ++j)
				{
					if(!removedIndeces.contains(j) && comparer.compare(ind, parent[j]) < 0)
					{
						newPopulation[i++] = ind.cloneIndividual(true);
						removedIndeces.add(j);
						//parents.remove(j);
						break;
					}
				}
			}
			for(; i < parent.length; ++i)
			{
				newPopulation[i] = parent[i];
			}
		}
		else
			throw new RuntimeException("unrecognised reinsertion strategy");
		
		return newPopulation;
	}
}
