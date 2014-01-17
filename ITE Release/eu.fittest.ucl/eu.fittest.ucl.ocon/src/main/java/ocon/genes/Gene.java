package ocon.genes;

import java.util.Random;

public abstract class Gene {
	
	protected final String identifier;
	
	public Gene(String identifier)
	{
		this.identifier = identifier;
	}
	public Gene(Gene seed)
	{
		this.identifier = seed.identifier;
	}
	public abstract void mutateGene(Random rnd);
	public abstract void initialize(Random rnd);
	public abstract String geneValueToString();
	public abstract Gene cloneGene();
	public String getIdentifier()
	{
		return this.identifier;
	}
}
