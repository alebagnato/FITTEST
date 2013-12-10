package ocon.genes;

import java.util.Random;

public class IntGene extends Gene {
	
	public int minValue;
	public int maxValue;
	public int defaultValue;
	public int geneValue;
	
	public IntGene(String _identifier, int min, int max, int defaultValue)
	{
		super(_identifier);
		this.minValue = min;
		this.maxValue = max;
		this.defaultValue = this.geneValue = defaultValue;
	}
	public IntGene(IntGene seed)
	{
		super(seed);
		this.minValue = seed.minValue;
		this.maxValue = seed.maxValue;
		this.defaultValue = seed.defaultValue;
		this.geneValue = seed.geneValue;
	}
	public void initialize(Random rnd)
	{
		this.geneValue = (int)(rnd.nextDouble() * (this.maxValue - this.minValue)) + this.minValue;
	}
	public IntGene cloneGene()
	{
		return new IntGene(this);
	}
	public void mutateGene(Random rnd)
	{
		this.geneValue = (int)(rnd.nextDouble() * (this.maxValue - this.minValue)) + this.minValue;
	}
	public String geneValueToString()
	{
		return String.valueOf(this.geneValue);
	}
}
