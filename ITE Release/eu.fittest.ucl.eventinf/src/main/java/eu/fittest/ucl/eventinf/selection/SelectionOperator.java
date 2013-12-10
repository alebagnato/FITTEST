package eu.fittest.ucl.eventinf.selection;

import java.util.ArrayList;
import java.util.Comparator;

import eu.fittest.ucl.eventinf.individual.Individual;

public interface SelectionOperator {
	
	public ArrayList<Individual> getParents(
			Individual[] population, 
			Comparator<Individual> comparer);
}
