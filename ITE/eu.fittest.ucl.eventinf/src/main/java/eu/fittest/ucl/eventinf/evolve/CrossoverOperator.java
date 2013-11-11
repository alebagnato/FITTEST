package eu.fittest.ucl.eventinf.evolve;

import java.util.ArrayList;
import java.util.Random;

import eu.fbk.se.fsm.FSM;
import eu.fbk.se.fsm.FSMAlgo;
import eu.fbk.se.fsm.MaxFSMSizeExceededException;
import eu.fittest.ucl.eventinf.individual.Individual;


public class CrossoverOperator {

	enum CrossoverOperation {uniteFSM, intersectFSM};

	private final int crossoverProbability;
	
	private final int matingPopulationSize;
	private final boolean sampleWithReplacement;
	
	private final Random ran;
	private final FSMAlgo fsmAlgo = new FSMAlgo();
	private int MAX_FSM_SIZE = 10000;
	
	private FSM uniteFSMs(FSM fsm1, FSM fsm2) throws MaxFSMSizeExceededException {
		
		FSM fsm = fsmAlgo.union(fsm1, fsm2);
		fsm = fsmAlgo.makeDeterministic(fsm, this.MAX_FSM_SIZE);
		fsm.regenerateLabels();
		
		return fsm;
	}
	
	private FSM intersectFSMs(FSM fsm1, FSM fsm2) throws MaxFSMSizeExceededException {
		
		FSM fsm = fsmAlgo.intersection(fsm1, fsm2);
		fsm = fsmAlgo.makeDeterministic(fsm, this.MAX_FSM_SIZE);
		fsm.regenerateLabels();
		
		return fsm;
	}
	
	
	public CrossoverOperator(Random ran, int matingPopulationSize, boolean sampleWithReplacement,
			int crossoverProbability, 
			int maxFSMSize)
	{
		this.MAX_FSM_SIZE = maxFSMSize;
		this.ran = ran;
		this.matingPopulationSize = matingPopulationSize;
		this.sampleWithReplacement = sampleWithReplacement;
		this.crossoverProbability = crossoverProbability;
	}

	private int[] generateTwoIndices(int maxLength)
	{
		int[] indices = new int[2];
		
		indices[0] = this.ran.nextInt(maxLength);
		
		while((indices[1] = this.ran.nextInt(maxLength)) == indices[0]){}
		
		return indices;
	}
	
	private Individual crossover(Individual parent1, Individual parent2, CrossoverOperation op)
	{
		Individual offspring = parent1.cloneIndividual(false);
		
		switch(op)
		{
			case uniteFSM:
				try {
					offspring.setFSM(uniteFSMs(parent1.getFSM(), parent2.getFSM()));
				} catch (MaxFSMSizeExceededException e) {
					// use parent 1
				}
				break;
			case intersectFSM:
				try {
					offspring.setFSM(intersectFSMs(parent1.getFSM(), parent2.getFSM()));
				} catch (MaxFSMSizeExceededException e) {
					// use parent 1
				}
				break;
				default:;
		}
		
		return offspring;
	}

	public ArrayList<Individual> crossover(ArrayList<Individual> parents) 
	{
		CrossoverOperation[] values = CrossoverOperation.values();
		
		ArrayList<Individual> offspringPopulation = new ArrayList<Individual>();
		
		if(parents.size() < 2)
		{
			return parents;
		}
		
		Individual parent1 = null;
		Individual parent2 = null;
				
		while(offspringPopulation.size() < this.matingPopulationSize && parents.size() >= 2)
		{
			int[] indices = generateTwoIndices(parents.size());
			
			parent1 = parents.get(indices[0]); 
			parent2 = parents.get(indices[1]); 
			
			if(this.sampleWithReplacement == false)
			{
				parents.remove(parent1);
				parents.remove(parent2);
			}
			
			if (ran.nextInt(100) < this.crossoverProbability)
			{
				offspringPopulation.add(crossover(parent1, parent2, 
						values[ran.nextInt(values.length)]));
			}
			else
			{
				offspringPopulation.add(parent1);
				if (offspringPopulation.size() < this.matingPopulationSize)
					offspringPopulation.add(parent2);
			}
		}
		
		return offspringPopulation;
	}
}
