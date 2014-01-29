package eu.fittest.ucl.eventinf.selection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import eu.fittest.ucl.eventinf.individual.Individual;
import eu.fittest.ucl.eventinf.selection.SelectionOperator;

public class Elitist implements SelectionOperator {

	private int matingPopulationSize;
	public Elitist(int matingPopulationSize)
	{
		this.matingPopulationSize = matingPopulationSize;
	}
	private ArrayList<Individual> getSortedPopulation(Individual[] population,
			Comparator<Individual> comparer)
	{
		ArrayList<Individual> sorted = new ArrayList<Individual>();
		for(Individual ind : population)
		{
			sorted.add(ind);
		}
		
		Collections.sort(sorted, comparer);
		
		return sorted;
	}

	public ArrayList<Individual> getParents(Individual[] population,
			Comparator<Individual> comparer) {
		// TODO Auto-generated method stub
		ArrayList<Individual> sorted = getSortedPopulation(population, comparer);
		ArrayList<Individual> parents = new ArrayList<Individual>();
		for(int i = 0; i < matingPopulationSize && i < sorted.size(); ++i)
		{
			parents.add(sorted.get(i));
		}
		return parents;
	}

}
