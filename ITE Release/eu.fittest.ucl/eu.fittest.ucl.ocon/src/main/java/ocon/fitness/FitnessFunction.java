package ocon.fitness;

import java.util.Comparator;

import ocon.individual.Individual;

public interface FitnessFunction extends Comparator<Individual> {
	public void evaluate(Individual ind);
	public int compare(Individual i1, Individual i2);
}
