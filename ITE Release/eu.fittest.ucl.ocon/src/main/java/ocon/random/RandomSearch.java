package ocon.random;

import java.util.Random;

import eu.fittest.component.optimizer.IOptimizerRequiredServices;

import ocon.entry.BestIndividualSetter;
import ocon.fitness.FitnessFunction;
import ocon.individual.Individual;
import ocon.individual.KingPropertyIndividual;
import ocon.search.SearchMethod;

public class RandomSearch extends SearchMethod {
			
	private final int maxRuns;
	
	public RandomSearch(IOptimizerRequiredServices requiredServices, Random rnd, FitnessFunction ff, BestIndividualSetter iBestIndSetter, int maxRuns)
	{
		super(requiredServices, rnd, ff, iBestIndSetter);
		this.maxRuns = maxRuns;
	}
	
	public void run()
	{		
		while(super.terminate == false && 
				super.numEvaluations < this.maxRuns)
		{
			
			Individual ind = new KingPropertyIndividual(super.random);
			super.fitnessFunction.evaluate(ind);
			if(ind.isIdeal())
			{
				super.bestIndividual = ind.cloneIndividual(true);
				super.iBestIndSetter.setBestIndividual(ind);
				super.terminate = true;
				break;
			}
			else if(super.bestIndividual == null || 
					super.fitnessFunction.compare(ind, super.bestIndividual) < 0)
			{
				super.bestIndividual = ind.cloneIndividual(true);
				super.iBestIndSetter.setBestIndividual(ind);
			}
			super.numEvaluations++;
			super.setProgress((int)((this.numEvaluations * 100) / this.maxRuns));
			if(super.DEBUG)
				System.out.println("Evaluation: " + super.numEvaluations);
		}
	}
}
