package ocon.evolve.selection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Random;

import ocon.individual.Individual;

class Segment
{
	double lowerBound;
	double upperBound;
	Individual ind;
	public Segment(double l, double u, Individual ind)
	{
		this.lowerBound = l;
		this.upperBound = u;
		this.ind = ind;
	}
}
class RankedComparer implements Comparator<Individual>
{
	public int compare(Individual o1, Individual o2) {
		// TODO Auto-generated method stub
		if(o1.getFitness() > o2.getFitness())
			return -1;
		else if(o1.getFitness() < o2.getFitness())
			return 1;
		else
			return 0;
	}
	
}
public class RouletteWheel implements SelectionStrategy {

	private double selectionPressure;
	private int trials;
	private Random random;
	
	public RouletteWheel(Random rnd, double sp, int trials)
	{
		this.random = rnd;
		this.selectionPressure = sp;
		this.trials = trials;
	}
	private ArrayList<Individual> clonePopulation(ArrayList<Individual> population)
	{
		ArrayList<Individual> clone = new ArrayList<Individual>(population.size());
		for(Individual ind : population)
		{
			clone.add(ind.cloneIndividual(true));
		}
		return clone;
	}
	private void linearRankPopulation(ArrayList<Individual> population, Comparator<Individual> comparer)
	{
		Collections.sort(population, comparer); //sort from best to worst
		Collections.reverse(population); //so worst individual is at position 1
		int ninds = population.size();
		for(int i = 0; i < ninds; ++i)
		{
			double ratio = (double)i /  (double)(ninds - 1);
			double rank = (2 - this.selectionPressure) + 2*(this.selectionPressure - 1)*ratio;
			population.get(i).assignFitness(rank);
		}
	}
	private ArrayList<Segment> computeSegmentPopulation(ArrayList<Individual> ranked)
	{
		Collections.sort(ranked, new RankedComparer()); //best fitness is at pos 1
		ArrayList<Segment> segmentPopulation = new ArrayList<Segment>(ranked.size());
		int size = ranked.size();
		double lower = 0.0;
		double upper = 0.0;
		for(Individual ind : ranked)
		{
			upper += (ind.getFitness() / size);
			segmentPopulation.add(new Segment(lower, upper, ind));
			lower = upper;
		}
		return segmentPopulation;
	}
	private Individual findSegmentIndividual(double pointer, ArrayList<Segment> segmentPopulation)
	{
		for(Segment seg : segmentPopulation)
		{
			if(pointer >= seg.lowerBound && pointer < seg.upperBound)
				return seg.ind;
		}
		return null;
	}
	public ArrayList<Individual> getParents(ArrayList<Individual> population,
			Comparator<Individual> comparer) {
		ArrayList<Individual> clone = clonePopulation(population);
		linearRankPopulation(clone, comparer);
		ArrayList<Segment> segmentPopulation = computeSegmentPopulation(clone);
		ArrayList<Individual> mating = new ArrayList<Individual>(this.trials);
		for(int i = 0; i < this.trials; ++i)
		{
			double pointer = this.random.nextDouble();
			Individual ind = findSegmentIndividual(pointer, segmentPopulation);
			assert(ind != null);
			if(!mating.contains(ind))
				mating.add(ind);
		}
		return mating;
	}

}
