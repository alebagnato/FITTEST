package ocon.individual;

public interface Individual {
	public void defaultCrossover(Individual ind2);
	public void defaultMutate();
	public void assignFitness(double fitnessValue);
	public double getFitness();
	public boolean isIdeal();
	public boolean isEvaluated();
	public Individual cloneIndividual(boolean keepFitnessValue);
	public String toString();
}
