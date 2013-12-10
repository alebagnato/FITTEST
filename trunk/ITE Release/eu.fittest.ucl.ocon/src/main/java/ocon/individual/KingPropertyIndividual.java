package ocon.individual;

import java.util.ArrayList;
import java.util.Random;

import ocon.genes.BooleanGene;
import ocon.genes.CollectionGene;
import ocon.genes.Gene;
import ocon.genes.IntGene;

public class KingPropertyIndividual implements Individual {
	
	public double mutationProbability = 0.01;
	public ArrayList<Gene> chromosome;
	public int numberOfGenes = 9;
	
	public double fitness = 0;
	public boolean evaluted = false;
	
	private Random random;
	
	private void buildChromosome()
	{
		//chromosome.add(new IntGene("executions", 1, 100, 2)); //number of times to execute this chromosome
		chromosome.add(new IntGene("executions", 3, 3, 3)); //number of times to execute this chromosome, lowerbound, upperbound, default
		chromosome.add(new IntGene("-Dcontest.noiseFrequency", 0, 1000, 70)); //noiseFrequency
		chromosome.add(new CollectionGene<String>("-Dcontest.noiseType", new String[]{"yields", "sleeps", "waits", "synchYields", "busyWait", "mixed"})); //noiseType
		chromosome.add(new IntGene("-Dcontest.strength", -1, 100, -1)); //strength
		chromosome.add(new BooleanGene("-Dcontest.haltOneThread", false)); //haltOneThread
		chromosome.add(new BooleanGene("-Dcontest.timeoutTampering", false)); //timeoutTampering
		chromosome.add(new BooleanGene("-Dcontest.sharedVarNoise", false)); //sharedVarNoise
		chromosome.add(new BooleanGene("-Dcontest.shared", false)); //shared
		chromosome.add(new BooleanGene("-Dcontest.nonVarNoise", false)); //nonVarNoise
	}
	private void repairGene(Gene gene)
	{
		if(gene.getIdentifier().equals("-Dcontest.strength"))
		{
			while(((IntGene)gene).geneValue == 0)
			{
				gene.mutateGene(this.random);
			}
		}
	}
	
	public KingPropertyIndividual(Random rnd)
	{
		this.random = rnd;
		
		this.chromosome = new ArrayList<Gene>(this.numberOfGenes);
		
		buildChromosome();
		
		for(Gene gene : chromosome)
		{
			gene.initialize(this.random);
			repairGene(gene);
		}
	}
	public KingPropertyIndividual(KingPropertyIndividual seed)
	{
		this.random = seed.random;
		this.fitness = seed.fitness;
		this.evaluted = seed.evaluted;
		this.chromosome = new ArrayList<Gene>(this.numberOfGenes);
		for(Gene gene : seed.chromosome)
		{
			this.chromosome.add(gene.cloneGene());
		}
	}
	
	public void defaultCrossover(Individual ind)
	{
		KingPropertyIndividual ind2 = (KingPropertyIndividual)ind;
		int size = this.chromosome.size();
		assert(size == ind2.chromosome.size());
		int xpoint = this.random.nextInt(size);
		for(int i = 0; i < xpoint; ++i)
		{
			Gene tmp = ind2.chromosome.get(i);
			ind2.chromosome.set(i, this.chromosome.get(i).cloneGene());
			this.chromosome.set(i, tmp.cloneGene());
		}
	}
	public void defaultMutate()
	{
		if(this.mutationProbability > 0.0)
			for(int i = 0; i < this.chromosome.size(); ++i)
	            if (this.random.nextGaussian() < this.mutationProbability)
	            {
	                Gene gene = this.chromosome.get(i);
	            	gene.mutateGene(this.random);
	            	repairGene(gene);
	            	break;
	            }
	}
	public Individual cloneIndividual(boolean keepFitnessValue)
	{
		KingPropertyIndividual ind =  new KingPropertyIndividual(this);
		if(!keepFitnessValue)
		{
			ind.fitness = 0;
			ind.evaluted = false;
		}
		return ind;
	}
	public double getFitness()
	{
		return this.fitness;
	}
	public void assignFitness(double fitnessValue)
	{
		this.fitness = fitnessValue;
	}
	public boolean isIdeal()
	{
		return this.fitness >= Integer.MAX_VALUE;
	}
	public boolean isEvaluated()
	{
		return this.evaluted;
	}
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		for(Gene gene : this.chromosome)
		{
			sb.append(gene.getIdentifier() + "=" + gene.geneValueToString() + "\n");
		}
		return sb.toString();
	}
	public String geneToString(String name)
	{
		for(Gene g : this.chromosome)
		{
			if(g.getIdentifier().equals(name))
			{
				return g.geneValueToString();
			}
		}
		return null;
	}
}
