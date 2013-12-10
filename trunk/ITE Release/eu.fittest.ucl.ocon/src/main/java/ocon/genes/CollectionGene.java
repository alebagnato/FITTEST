package ocon.genes;

import java.util.ArrayList;
import java.util.Random;

public class CollectionGene<T> extends Gene {

	public ArrayList<T> items;
	public T geneValue;
	
	public CollectionGene(String _identifier, T[] _items)
	{
		super(_identifier);
		if(_items == null)
		{
			this.items = new ArrayList<T>();
		}
		else
		{
			this.items = new ArrayList<T>(_items.length);
			for(T item : _items)
			{
				this.items.add(item);
			}
			
			if(_items.length > 0)
				this.geneValue = _items[0];
		}
	}
	public CollectionGene(CollectionGene<T> seed)
	{
		super(seed);
		this.items = new ArrayList<T>(seed.items);
		this.geneValue = seed.geneValue;
	}
	public void initialize(Random rnd)
	{
		this.geneValue = this.items.get(rnd.nextInt(this.items.size()));
	}
	
	public CollectionGene<T> cloneGene()
	{
		return new CollectionGene<T>(this);
	}
	public void mutateGene(Random rnd)
	{
		initialize(rnd);
	}
	public String geneValueToString()
	{
		return String.valueOf(this.geneValue);
	}
}
