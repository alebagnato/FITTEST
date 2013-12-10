package ocon.search;

import java.util.Random;

import eu.fittest.component.optimizer.IOptimizerRequiredServices;

import ocon.entry.BestIndividualSetter;
import ocon.fitness.FitnessFunction;
import ocon.individual.Individual;

public abstract class SearchMethod extends Thread {
	
	protected boolean DEBUG;
	protected boolean terminate;
	protected long numEvaluations;
	
	protected Individual bestIndividual;
	
	protected final Random random;
	protected final FitnessFunction fitnessFunction;
	protected final BestIndividualSetter iBestIndSetter;
	protected final IOptimizerRequiredServices requiredServices;
	
	protected SearchMethod(IOptimizerRequiredServices requiredServices, Random rnd, FitnessFunction fit, BestIndividualSetter setter)
	{
		this.requiredServices = requiredServices;
		this.random = rnd;
		this.fitnessFunction = fit;
		this.iBestIndSetter = setter;
		this.terminate = false;
		this.DEBUG = true;
		this.numEvaluations = 0;
		
		this.bestIndividual = null;
	}
	
	protected void setProgress(int progress)
	{
		if(this.requiredServices != null)
			this.requiredServices.setProgress(progress);
	}
	
 	public void stopRun()
	{
		this.terminate = true;
	}
	public void debug(boolean enableDebug)
	{
		this.DEBUG = enableDebug;
	}
}
