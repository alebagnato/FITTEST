package ocon.entry;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;
import java.util.Random;

import ocon.evolve.SimpleGA;
import ocon.evolve.selection.RouletteWheel;
import ocon.fitness.ConTestCoverage;
import ocon.fitness.FitnessFunction;
import ocon.genes.Gene;
import ocon.individual.Individual;
import ocon.individual.KingPropertyIndividual;
import ocon.random.RandomSearch;
import ocon.search.SearchMethod;
import eu.fittest.component.optimizer.IOptimizerProvidedServices;
import eu.fittest.component.optimizer.IOptimizerRequiredServices;
import eu.fittest.component.optimizer.SearchAlgorithm;

public class OptimizeKingProperties implements BestIndividualSetter, IOptimizerProvidedServices {
	public static final int DEFAULT_POP_SIZE = 100;
	public static final int DEFAULT_OFFSPRING = 10;
	public static final int DEFAULT_MAX_GENERATIONS = 100;
	public static final long DEFAULT_MAX_RUNTIME = -1;
	public static final int DEFAULT_MAX_EVALUATIONS = 100;
	public static final long DEFAULT_SEED = 0;
	public static final int DEFAULT_DEBUG_LEVEL = 1;
	public static final SearchAlgorithm DEFAULT_SEARCH_METHOD = SearchAlgorithm.GeneticAlgorithm;

	private final Random random;
	private final long maxRuntime;
	private SearchAlgorithm searchMethod;
	
	private IOptimizerRequiredServices rs;
	private File outputFolder;
	private long randomNumberSeed;
	private Properties seedKingProperties;
	
	private Individual bestProperties;
	private int debugLevel;
	
	public void setDebugLevel(int level)
	{
		this.debugLevel = level;
	}
	
	public synchronized void setBestIndividual(Individual ind)
	{
		this.bestProperties = ind.cloneIndividual(true);
	}
	
	public Long getRandomNumberSeed()
	{
		return this.randomNumberSeed;
	}
	
	public OptimizeKingProperties()
	{
		this.random = new Random();
		this.maxRuntime = DEFAULT_MAX_RUNTIME;
		this.searchMethod = DEFAULT_SEARCH_METHOD;
		this.debugLevel = DEFAULT_DEBUG_LEVEL;
	}
	
	private SearchMethod setupGA(FitnessFunction ff)
	{
		int popSize = DEFAULT_POP_SIZE;
		int offspring = DEFAULT_OFFSPRING;
		int maxGenerations = DEFAULT_MAX_GENERATIONS;
				
		RouletteWheel rouletteSelection = new RouletteWheel(this.random, 2.0, offspring);
		SimpleGA search = new SimpleGA(this.rs, this.random, ff, this, popSize, maxGenerations, rouletteSelection);
		
		return search;
	}
	
	private SearchMethod setupRandom(FitnessFunction ff)
	{
		int maxRuns = DEFAULT_MAX_EVALUATIONS;
		RandomSearch search = new RandomSearch(this.rs, this.random, ff, this, maxRuns);
		return search;
	}
	
	private void setupAndRunRemote(SearchMethod searchMethod)
	{							
		Thread t = new Thread(searchMethod);
		t.start();
		try {
			t.join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		saveBestConfiguration();
		rs.setProgress(100);//completed only after saving configuration
	}
	
	private void setupAndRunLocal(SearchMethod searchMethod)
	{							
		Thread t = new Thread(searchMethod);
		t.start();
		
		try
		{
			if(this.maxRuntime == -1) //wait til maxGenerations
				t.join();
			else
			{
				t.join(this.maxRuntime);
				boolean isSet = false;
				while(t.isAlive())
				{
					if(!isSet)
					{
						searchMethod.stopRun();
						isSet = true;
					}
				}
			}
		}
		catch(InterruptedException e)
		{
			e.printStackTrace();
		}
		finally
		{
			saveBestConfiguration();
		}
	}
		
	private void saveBestConfiguration()
	{
		if(this.bestProperties != null)
		{
			FileWriter writer = null;
//			BufferedWriter output = null;
			KingPropertyIndividual ind = (KingPropertyIndividual)this.bestProperties;
			
			try {
				File newKingProperties = new File(this.outputFolder, "KingProperties");
				if(!newKingProperties.getParentFile().exists() && !newKingProperties.getParentFile().mkdirs()) throw new FileNotFoundException(newKingProperties.getParentFile().toString());
				
				writer = new FileWriter(newKingProperties);
				//output = new BufferedWriter(writer);
				
				int prefixLength = "-Dcontest.".length();
				
				for(Gene gene : ind.chromosome)
				{
					if(gene.getIdentifier().startsWith("-Dcontest"))
					{
						String key = gene.getIdentifier().substring(prefixLength);
						this.seedKingProperties.setProperty(key, gene.geneValueToString());
					}
				}
				
				this.seedKingProperties.store(writer, "Optimized King Properties");
				
				/*for(String key : this.seedKingProperties.stringPropertyNames())
				{
					if(key.equals("noiseFrequency"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.noiseFrequency"));
					}
					else if(key.equals("random"))
					{
						output.write("random = false");
					}
					else if(key.equals("noiseType"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.noiseType"));
					}
					else if(key.equals("strength"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.strength"));
					}
					else if(key.equals("haltOneThread"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.haltOneThread"));
					}
					else if(key.equals("timeoutTampering"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.timeoutTampering"));
					}
					else if(key.equals("sharedVarNoise"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.sharedVarNoise"));
					}
					else if(key.equals("shared"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.shared"));
					}
					else if(key.equals("nonVarNoise"))
					{
						output.write(key + " = " + 
								ind.geneToString("-Dcontest.nonVarNoise"));
					}
					else
					{
						String value = this.seedKingProperties.getProperty(key);
						output.write(key + " = " + value); 
					}
					output.write(newline);
				}*/
				
				writer.flush();
				writer.close();
				
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		//System.exit(0);
	}
	
	@Override
	public void setOutputFolder(File arg0) {
		// TODO Auto-generated method stub
		this.outputFolder = arg0;
	}
	@Override
	public void setSeedProperties(Properties arg0) {
		// TODO Auto-generated method stub
		this.seedKingProperties = arg0;
	}
	@Override
	public void setRandomNumberSeed(Long seed) {
		// TODO Auto-generated method stub
		this.randomNumberSeed = seed;
		this.random.setSeed(seed);
	}
	@Override
	public void setSearchAlgorithm(SearchAlgorithm value) {
		// TODO Auto-generated method stub
		this.searchMethod = value;
	}
	@Override
	public void setOptimizerRequiredServices(IOptimizerRequiredServices requiredServices) {
		this.rs = requiredServices;
	}
	
	@Override
	public void interrupt() {
		_method.stopRun();
	}
		
	private SearchMethod _method;
	
	@Override
	public void run() {
				
		FitnessFunction fitness = new ConTestCoverage(this.rs, this.seedKingProperties);
		
		if(this.searchMethod == SearchAlgorithm.RandomSearch)
			_method = setupRandom(fitness);
		else
			_method = setupGA(fitness);
		
		switch(this.debugLevel)
		{
		case 0:
		default:
			_method.debug(false);
			ConTestCoverage.DEBUG = false;
			break;
		case 1:
			_method.debug(true);
			ConTestCoverage.DEBUG = false;
			break;
		case 2:
			_method.debug(true);
			ConTestCoverage.DEBUG = true;
			break;
		}
		
		if(this.rs == null)
			setupAndRunLocal(_method);
		else
		{
			setupAndRunRemote(_method);
		}
	}

}
