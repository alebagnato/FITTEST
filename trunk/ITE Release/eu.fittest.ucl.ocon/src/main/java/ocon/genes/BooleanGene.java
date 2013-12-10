package ocon.genes;

import java.util.Random;

public class BooleanGene extends Gene {
	
	public boolean defaultValue;
	public boolean geneValue;
	
	public BooleanGene(String _identifier, boolean defaultValue)
	{
		super(_identifier);
		this.defaultValue = this.geneValue = defaultValue;
	}
	public BooleanGene(BooleanGene seed)
	{
		super(seed);
		this.defaultValue = seed.defaultValue;
		this.geneValue = seed.geneValue;
	}
	public void initialize(Random rnd)
	{
		this.geneValue = rnd.nextBoolean();
	}
	public BooleanGene cloneGene()
	{
		return new BooleanGene(this);
	}
	public void mutateGene(Random rnd)
	{
		this.geneValue = !this.geneValue;
	}
	public String geneValueToString()
	{
		return String.valueOf(this.geneValue);
	}
}
