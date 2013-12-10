package eu.fittest.ucl.eventinf.selection;


import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Random;

import eu.fittest.ucl.eventinf.individual.Individual;
import eu.fittest.ucl.eventinf.selection.RankedComparer;
import eu.fittest.ucl.eventinf.selection.Segment;
import eu.fittest.ucl.eventinf.selection.SelectionOperator;

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
public class RouletteWheel implements SelectionOperator {

	private double selectionPressure;
	private int matingPopulationSize;
	private final Random random;
	
	public RouletteWheel(Random rnd, double sp, int matingPopulationSize)
	{
		this.random = rnd;
		this.selectionPressure = sp;
		this.matingPopulationSize = matingPopulationSize;
	}
	private ArrayList<Individual> clonePopulation(Individual[] population)
	{
		ArrayList<Individual> clone = new ArrayList<Individual>(population.length);
		for(int i = 0; i < population.length; ++i)
		{
			clone.add(population[i].cloneIndividual(true));
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
	public ArrayList<Individual> getParents(Individual[] population,
			Comparator<Individual> comparer) {
		ArrayList<Individual> clone = clonePopulation(population);
		linearRankPopulation(clone, comparer);
		ArrayList<Segment> segmentPopulation = computeSegmentPopulation(clone);
		ArrayList<Individual> mating = new ArrayList<Individual>(this.matingPopulationSize);
		while(mating.size() < this.matingPopulationSize)
		{
			double pointer = this.random.nextDouble();
			Individual ind = findSegmentIndividual(pointer, segmentPopulation);
			assert(ind != null);
			mating.add(ind);
		}
		return mating;
	}

}

