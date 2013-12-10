package ocon.fitness;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ocon.genes.Gene;
import ocon.genes.IntGene;
import ocon.individual.Individual;
import ocon.individual.KingPropertyIndividual;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.component.optimizer.IOptimizerRequiredServices;

public class ConTestCoverage implements FitnessFunction {
		
	public static boolean DEBUG = true;
	
	private enum CoverageType{location, sync};
	private Pattern syncCoveragePattern;
	private Pattern eventPairPattern;
		
	private final String syncFolder = "synchronization";
	private final String locFolder = "locationPairsTraces";
	
	private final IOptimizerRequiredServices requiredServices;
	private final Properties kingProperties;
	
	private final int propertyStartIndex =  "-Dcontest.".length();
	
	public void setProgress(int v)
	{
		if(this.requiredServices != null)
			this.requiredServices.setProgress(v);
	}
	
	public ConTestCoverage(IOptimizerRequiredServices requiredServices, Properties kingProperties)
	{
		this.requiredServices = requiredServices;
		this.kingProperties = kingProperties;
		
		this.syncCoveragePattern = Pattern.compile("(\\bsynchronization\\s(visited|blocked|blocking)|\\bwait\\s(visited|repeated))");
		this.eventPairPattern = Pattern.compile("(\\bf\\b|\\bt\\b)");
	}
	
	private File[] getTraceFiles(File dir)
	{
		FilenameFilter filter = new FilenameFilter() { public boolean accept(File dir, String name) { return name.endsWith(".trace"); } }; 
		return dir.listFiles(filter);
	}
	private void deleteTraceFiles(File[] files)
	{
		for(File f : files)
		{
			if(f.exists())
				f.delete();
		}
	}
	private double countWeightedLinesInFile(File f, CoverageType covType)
	{
		double locs = 0;
		
		try {
			FileReader fReader = new FileReader(f);
			BufferedReader input = new BufferedReader(fReader);
			try {
				String line = null;
				while ((line = input.readLine()) != null) {
					if (!line.isEmpty())
					{
						locs += getLineWeight(line, covType);
					}
				}
				input.close();
				fReader.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return locs;
	}
	private double getLineWeight(String line, CoverageType covType)
	{
		Matcher m;
		switch(covType)
		{
		case location:
			m = this.eventPairPattern.matcher(line);
			while(m.find())
			{
				if(m.group().equals("f"))
				{
					return 1;
				}
				else
				{
					return 10;
				}
			}
			return 0;
		case sync:
			m = this.syncCoveragePattern.matcher(line);
			while(m.find())
			{
				if(m.group().endsWith("visited") || m.group().endsWith("repeated"))
				{
					return 1000;
				}
				else
				{
					return 100;
				}
			}
			return 0;
		default: throw new RuntimeException("invalid coverage type");
		}
	}
	private double parseAndDeleteTraceFile(File dir, CoverageType covType)
	{
		File[] traces = getTraceFiles(dir);
		if(traces == null)
			return 0;
		double weightedLines = 0;
		for(File f : traces)
		{
			weightedLines += countWeightedLinesInFile(f, covType);
		}
		if(this.requiredServices == null)
			deleteTraceFiles(traces);
		return weightedLines;
	}
	private double parseTraceFiles(File baseDir)
	{
		double locCoverage = parseAndDeleteTraceFile(new File(baseDir, this.locFolder), CoverageType.location);
		double syncCoverage = parseAndDeleteTraceFile(new File(baseDir, this.syncFolder), CoverageType.sync);
		return (locCoverage + syncCoverage);
	}
		
	private double executeLocal(KingPropertyIndividual kind)
	{
		throw new UnsupportedOperationException("Execute local fitness function");/*
		int numberofExec = 0;
		StringBuilder sb = new StringBuilder();
		
		for(Gene gene : kind.chromosome)
		{
			if(gene.getIdentifier().equals("executions"))
			{
				IntGene ng = (IntGene)gene;
				numberofExec = ng.geneValue;
			}
			else if(gene.getIdentifier().startsWith("-Dcontest"))
				sb.append(gene.getIdentifier() + "=" + gene.geneValueToString() + " ");
		}
		
		String command = "java";
		if(!this.userClasspaths.isEmpty())
			command += " -classpath " + this.userClasspaths;
		command += " -javaagent:" + this.pathToConTestJar + 
				   " -Dcontest.random=false " + sb.toString() + 
				   " -Dcontest.targetClasses=" + this.targetClasses + " " + 
				   this.testSuiteClass;
				
		double fitnessSum = 0;
		for(int i = 0; i < numberofExec; ++i)
		{
			if(DEBUG)
				System.out.println("Executing:\n" + command);
			
			String output = Utils.sysExec(command, null, deadlockTimeout);
			
			if(DEBUG)
			{
				if(output == null)
					System.out.println("Either timeout (e.g. deadlock) interrupt exception");
				else
					System.out.println(output);
			}
			
			fitnessSum += parseTraceFiles(this.defaultLogDir);
			
			if(DEBUG)
				System.out.println("Fitness sum = " + fitnessSum);
		}
		//average fitness over number of runs
		return fitnessSum / numberofExec;*/
	}

	public void evaluate(Individual ind)
	{
		KingPropertyIndividual kind = (KingPropertyIndividual)ind;
		double fitness = 0;
		if(this.requiredServices == null)
			fitness = executeLocal(kind);
		else
		{
			int numberofExec = 0;
		
			for(Gene gene : kind.chromosome)
			{
				if(gene.getIdentifier().equals("executions"))
				{
					IntGene ng = (IntGene)gene;
					numberofExec = ng.geneValue;
				}
				else if(gene.getIdentifier().startsWith("-Dcontest"))
				{
					String key = gene.getIdentifier().substring(this.propertyStartIndex);
					this.kingProperties.setProperty(key, gene.geneValueToString());
				}
			}
			double fitnessSum = 0;
			for(int i = 0; i < numberofExec; ++i)
			{
				if(DEBUG){
					System.out.println("Execution number: "+i);
				}
				File logDir = null;
				try {
					logDir = this.requiredServices.execute(this.kingProperties);
					fitnessSum += parseTraceFiles(logDir);
				} catch (FITTESTException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			
			fitness = fitnessSum / numberofExec;
		}
		
		kind.fitness = fitness;
		kind.evaluted = true;
	}
	public int compare(Individual i1, Individual i2)
	{
		KingPropertyIndividual kind1 = (KingPropertyIndividual)i1;
		KingPropertyIndividual kind2 = (KingPropertyIndividual)i2;
		if(kind1.fitness > kind2.fitness)
			return -1; //better
		else if(kind2.fitness < kind2.fitness)
			return 1;
		else
			return 0;
	}
}
