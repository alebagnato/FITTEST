/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */
package eu.fittest.ucl.autoabs.evolve;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.Random;

import eu.fbk.se.fsm.FSM;
import eu.fittest.ucl.api.ModelInferenceListener;
import eu.fittest.ucl.autoabs.cluster.ClusterAlgo;
import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

/**
 * State Abstraction inference GA
 *
 */
public class InferenceGA {
	
	private ArrayList<Chromosome> population;
	private final int populationSize;
	private final int maxGenerations;
	private double mutationProbability;
	
	private int currentGeneration;
	private boolean terminate;
	private Chromosome bestSolution;
	private int bestGeneration;
	
	private final Random rnd;
	private final Fitness fitness;
	private final Selection selector;
	
	private final ClusterAlgo clusterAlgo;
	private final LinkedHashSet<ConcreteState> states;
	
	private final int maxGenerationsWithoutImprovement;
	private boolean addIdenticalChromosomesToArchive = false;
	private ArrayList<Chromosome> nonDominated;
	private final boolean useArchive;
	
	private final ArrayList<ModelInferenceListener> listeners;
	
	public InferenceGA(
			ArrayList<ModelInferenceListener> listeners,
			Random rnd, 
			Fitness fit, Selection selector, int populationSize, int maxGenerations,
			int maxGenerationsWithoutImprovement,
			ClusterAlgo clusterAlgo, LinkedHashSet<ConcreteState> states, 
			boolean useArchive, double mutationProbability)
	{
		this.maxGenerationsWithoutImprovement = maxGenerationsWithoutImprovement;
		this.listeners = listeners;
		this.rnd = rnd;
		this.selector = selector;
		this.fitness = fit;
		this.populationSize = populationSize;
		this.maxGenerations = maxGenerations;
		this.bestGeneration = this.currentGeneration = 0;
		this.bestSolution = null;
		
		this.clusterAlgo = clusterAlgo;
		this.states = states;
		this.population = new ArrayList<Chromosome>(this.populationSize);
		this.nonDominated = new ArrayList<Chromosome>();
		
		this.useArchive = useArchive;
		this.mutationProbability = mutationProbability;
	}
	
	private void reportProgress(double p, String msg) {
		String message = msg == null? "Evolving models":msg;
		for(ModelInferenceListener listener : this.listeners) {
			listener.progress(p, message);
		}
	}
	
	public void run()
	{
		this.initializePopulation();
		
		this.reportProgress(0, null);
		
		this.evaluatePopulation(this.population); //evaluate initial population
		
		while(this.terminate == false && 
				this.currentGeneration < this.maxGenerations)
		{
			ArrayList<Chromosome> mating = this.selector.getParents(this.population, 
					this.fitness);
			
			ArrayList<Chromosome> offspring = crossover(mating);
			
			mutate(offspring);
			
			evaluatePopulation(offspring);
			
			if(this.terminate) break;
			else if(this.currentGeneration - this.bestGeneration > this.maxGenerationsWithoutImprovement)
			{
				//System.out.println("Stopping, because no impromvement for " + (this.currentGeneration - this.bestGeneration) + " generations");
				this.reportProgress(100, "Stopping, because no impromvement for " + (this.currentGeneration - this.bestGeneration) + " generations");
				break;
			}
			
			updatePopulation(offspring);
			
			this.reportProgress((this.currentGeneration * 100) / this.maxGenerations, null);
		}
		
		this.reportProgress(100, "Finished");
		//remove duplicates from archive
		/*Iterator<Chromosome> it = this.nonDominated.iterator();
		ArrayList<Chromosome> unique = new ArrayList<Chromosome>(this.nonDominated.size());
		while(it.hasNext()) {
			Chromosome c = it.next();
			if(unique.isEmpty()) {
				unique.add(c);
			} else {
				boolean skip = false;
				for(Chromosome p : unique) {
					if(p.getFSMSize() == c.getFSMSize()
							&& p.getNDET() == c.getNDET()
							&& p.getInfeasible() == c.getInfeasible()) {
						skip = true;
						break;
					}
				}
				if(!skip)
					unique.add(c);
			}
		}
		this.nonDominated = new ArrayList<Chromosome>(unique);*/
	}
	
	/**
	 * Evaluate a set of individuals. Initially this will be the entire population, in 
	 * subsequent iterations it will be the offspring
	 * @param pop List of individuals to evaluate
	 */
	private void evaluatePopulation(ArrayList<Chromosome> pop)
	{
		this.currentGeneration++;
		
		int current = 0;
		while(current < pop.size() && !this.terminate)
		{
			Chromosome ind = pop.get(current);
			if(!ind.isEvaluated())
			{
				this.fitness.evaluate(ind);
				
				if(this.useArchive) {
					//check if ind dominates any individuals in the archive and remove all dominated individuals
					boolean add = true;
					for(int i = 0; i < this.nonDominated.size(); i++) {
						int res = this.fitness.compare(ind, this.nonDominated.get(i));
						if(res == 1)
						{
							add = false;
							break;
						}
						else if(res == -1) {
							//ind dominates a member of the archive, so remove the member
							this.nonDominated.remove(i);
							add = true;
							i--;
						} else { 
							if(this.addIdenticalChromosomesToArchive) {
								add = true;
							} else {
								//identical, so don't add to archive?
								if(this.fitness.areEqual(ind, this.nonDominated.get(i))) {
									add = false;
									break;
								} else {
									add = true;
								}
							}
						}
					}
					if(add || this.nonDominated.isEmpty()) {
						this.nonDominated.add(ind.cloneChromosome(true));
						this.bestGeneration = this.currentGeneration;
					}
				} else {
					if(this.bestSolution == null || 
							this.fitness.compare(ind, this.bestSolution) < 0)
					{
						this.bestSolution = ind.cloneChromosome(true);
						this.bestGeneration = this.currentGeneration;
					}
				}
			}
			current += 1;
		}
	}
	
	/**
	 * @param matingPop The mating population from which to pick parents for 
	 * crossover
	 * @return A list of offspring
	 */
	private ArrayList<Chromosome> crossover(ArrayList<Chromosome> matingPop)
	{
		int size = matingPop.size();
		int mates = size / 2;
		int[] indices = new int[2];
		int cnt = 0;
		ArrayList<Chromosome> offspring = new ArrayList<Chromosome>(size);
		while(cnt < mates)
		{
			this.selector.generateTwoIndices(indices, size);
			Chromosome p1 = matingPop.get(indices[0]);
			Chromosome p2 = matingPop.get(indices[1]);
			Chromosome o1 = p1.cloneChromosome(false);
			Chromosome o2 = p2.cloneChromosome(false);
			o1.defaultCrossover(o2);
			offspring.add(o1);
			offspring.add(o2);
			cnt += 1;
		}
		
		return offspring;
	}
	/**
	 * @param offspring A list of offspring to mutate
	 */
	private void mutate(ArrayList<Chromosome> offspring)
	{
		int size = offspring.size();
		int cnt = 0;
		while(cnt < size)
		{
			offspring.get(cnt).defaultMutate();
			cnt += 1;
		}
	}
	/**
	 * Elitist re-insertion operation. If an offspring is better than a current member of the population,
	 * then replace that member with the offspring
	 * @param offspring A list of offspring to possibly re-insert into the population.
	 */
	private void updatePopulation(ArrayList<Chromosome> offspring)
	{
		//TODO: sort offspring from best to worst
		Chromosome parent = null;
		Chromosome ind = null;
		for(int i = 0; i < this.population.size() && offspring.size() > 0; i++) {
			parent = this.population.get(i);
			for(int j = 0; j < offspring.size(); j++) {
				ind = offspring.get(j);
				int status = this.fitness.compare(ind, parent);
				if(status == 0) {
					if(!this.fitness.areEqual(ind, parent)) {
						//toss coin as to whether to accept or reject the child
						if(this.rnd.nextBoolean())
							status = -1;
					}
				}
				if(status < 0) {
					//remove parent
					parent.dispose();
					this.population.remove(i);
					i = -1;
					//append offspring to population
					this.population.add(ind.cloneChromosome(true));
					//remove offspring from list, so it doesn't get added again in the future
					offspring.remove(j);
					break;
				}
			}
		}
		parent = ind = null;
		for(Chromosome child : offspring) {
			child.dispose();
		}
		offspring.clear();
	}
	/**
	 * Generate the initial random population. Each chromosome choose a random 'k'-cut off point
	 * and uses the clustering from the dendrogram for that 'k'
	 */
	private void initializePopulation()
	{
		for(int i = 0; i < this.populationSize; ++i)
		{
			this.population.add(i, new Chromosome(this.rnd, this.clusterAlgo, this.states, this.mutationProbability));
		}
	}
	
		
	/**
	 * Helper function to save the current population to a set of FSM files
	 * @throws IOException
	 */
	public void saveParetoFrontToFile(File dir) {
		int counter = 0;
		PrintStream s = null;
		for(Chromosome c : this.nonDominated) {
			if(c.getFSM() == null) this.fitness.evaluate(c, true);
			try {
				String fsmName = "fsm_" + String.valueOf(counter) + ".fsm";
				//writer = new BufferedWriter(new FileWriter());
				s = new PrintStream(new File(dir, fsmName));
				c.getFSM().regenerateLabels();
				c.getFSM().print(s);
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				if(s != null) {
					s.flush();
					s.close();
				}
			}
			counter++;
		}
	}
		
	public FSM[] getNonDominatedFSM() {
		FSM[] fsms = new FSM[this.nonDominated.size()];
		int counter = 0;
		for(Chromosome c : this.nonDominated) {
			
			if(c.getFSM() == null) this.fitness.evaluate(c, true);
			
			fsms[counter] = c.getFSM();
			counter++;
		}
		
		return fsms;
	}
}
