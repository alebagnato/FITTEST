package eu.fittest.ucl.eventinf.selection;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

import eu.fittest.ucl.eventinf.individual.Individual;
import eu.fittest.ucl.eventinf.selection.SelectionOperator;

public class Tournament implements SelectionOperator {

	private final Random ran;
	private final int matingPopulationSize;
	public Tournament(Random ran, int matingPopulationSize)
	{
		this.ran = ran;
		this.matingPopulationSize = matingPopulationSize;
	}

	private ArrayList<Individual> runTournament(ArrayList<Individual> population, 
			Comparator<Individual> comparer)
	{
		ArrayList<Individual> winners = new ArrayList<Individual>();
		
		while(population.size() > 1)
		{
			int i1 = ran.nextInt(population.size());
			int i2 = ran.nextInt(population.size());
			while(i1 == i2) i2 = ran.nextInt(population.size());
			Individual ind1 = population.get(i1);
			Individual ind2 = population.get(i2);
			int result = comparer.compare(ind1, ind2);
			if(result == 0)
			{
				if(ran.nextInt() % 2 == 0)
				{
					winners.add(ind1);
					
				}
				else
				{
					winners.add(ind2);
				}
			}
			else if(result == -1)
			{
				winners.add(ind1);
			}
			else
				winners.add(ind2);
			
			population.remove(ind1);
			population.remove(ind2);
		}
		//add any non participating ind as winner
		for(Individual ind : population)
			winners.add(ind);
		return winners;
	}
	
	public ArrayList<Individual> getParents(Individual[] population,
			Comparator<Individual> comparer) {
		// TODO Auto-generated method stub
		
		ArrayList<Individual> parents = new ArrayList<Individual>();
		if(population.length < 2)
		{
			for(Individual ind : population)
				parents.add(ind);
			
			return parents;
		}
		
		ArrayList<Individual> sourcePopulation = new ArrayList<Individual>();
		for(Individual ind : population)
		{
			sourcePopulation.add(ind);
		}
				
		do
		{
			//get winners
			sourcePopulation = runTournament(sourcePopulation, comparer);
			//add all winners to mating population
			for(int i = 0; i < sourcePopulation.size() && 
							parents.size() < this.matingPopulationSize; ++i)
			{
				parents.add(sourcePopulation.get(i));
			}
			
		}while(parents.size() < this.matingPopulationSize && !sourcePopulation.isEmpty());
		/*		
		for(int i = 0; i < matingPopulationSize; ++i)
		{
			int i1 = ran.nextInt(population.length);
			int i2 = ran.nextInt(population.length);
			while(i1 == i2) i2 = ran.nextInt(population.length);
			int result = comparer.compare(population[i1], population[i2]);
			if(result == 0)
			{
				if(ran.nextInt() % 2 == 0)
					parents.add(population[i1]);
				else
					parents.add(population[i2]);
			}
			else if(result == -1)
			{
				parents.add(population[i1]);
			}
			else
				parents.add(population[i2]);
		}
		*/
		return parents;
	}

}
