package ocon.evolve.selection;

import java.util.ArrayList;
import java.util.Comparator;

import ocon.individual.Individual;

public interface SelectionStrategy {
	
	public ArrayList<Individual> getParents(ArrayList<Individual> population, Comparator<Individual> comparer);
}
