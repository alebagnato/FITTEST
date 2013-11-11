/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.evolve;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Random;

/**
 * Tournament selection
 *
 */
public class Selection {
	
	private int numTournaments;
	private final Random rnd;
	
	public Selection(Random rnd, int numTournaments)
	{
		this.rnd = rnd;
		this.numTournaments = numTournaments;
	}
	
	/**
	 * Generate two unique indices within range [0,limit[
	 * @param out_indices The array that will hold the two unique indices to be returned
	 * @param limit The max range for the indices to generate
	 */
	public void generateTwoIndices(int[] out_indices, int limit)
	{
		int _i1 = this.rnd.nextInt(limit);
		int _i2 = this.rnd.nextInt(limit);
		while(_i1 == _i2 && limit > 1)
		{
			_i2 = this.rnd.nextInt(limit);
		}
		out_indices[0] = _i1;
		out_indices[1] = _i2;
	}
	/**
	 * Run tournaments
	 * @param population List of parents
	 * @param comparer Comparer function to compare two parents and choose tournament winner
	 * @return A list of individuals to use for mating
	 */
	public ArrayList<Chromosome> getParents(ArrayList<Chromosome> population, Comparator<Chromosome> comparer)
	{
		int tournaments = 0;
		int[] indices = new int[2];
		int size = population.size();
		Chromosome c1, c2;
		ArrayList<Chromosome> mating = new ArrayList<Chromosome>(this.numTournaments);
		while(tournaments < this.numTournaments)
		{
			//pick two random individuals
			this.generateTwoIndices(indices, size);
			c1 = population.get(indices[0]);
			c2 = population.get(indices[1]);
			if(comparer.compare(c1, c2) < 0)
				mating.add(c1);
			else
				mating.add(c2);
			tournaments++;
		}
		
		return mating;
	}
}
