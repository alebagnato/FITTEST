package eu.fittest.ucl.api;

public class ModelInferenceParameters {
	public int getPopulationSize() {
		return populationSize;
	}

	public void setPopulationSize(int populationSize) {
		if(populationSize > 0) {
			this.populationSize = populationSize;
			this.numTournaments = (int)Math.floor(0.1 * this.populationSize);
		}
	}

	public int getMaximumGenerations() {
		return maximumGenerations;
	}

	public void setMaximumGenerations(int maximumGenerations) {
		this.maximumGenerations = maximumGenerations;
	}

	public int getNumTournaments() {
		return numTournaments;
	}

	public void setNumTournaments(int numTournaments) {
		this.numTournaments = numTournaments;
	}

	public double getMutationProbability() {
		return mutationProbability;
	}

	public void setMutationProbability(double mutationProbability) {
		this.mutationProbability = mutationProbability;
	}

	public boolean isSaveFinalPopulationToFile() {
		return saveFinalPopulationToFile;
	}

	public void setSaveFinalPopulationToFile(boolean saveFinalPopulationToFile) {
		this.saveFinalPopulationToFile = saveFinalPopulationToFile;
	}

	public boolean isUseMultiObjectiveGA() {
		return useMultiObjectiveGA;
	}

	public void setUseMultiObjectiveGA(boolean useMultiObjectiveGA) {
		this.useMultiObjectiveGA = useMultiObjectiveGA;
	}

	public long getRandomNumberSeed() {
		return randomNumberSeed;
	}

	public void setRandomNumberSeed(long randomNumberSeed) {
		this.randomNumberSeed = randomNumberSeed;
	}

	public int getNumMaxClusters() {
		return numMaxClusters;
	}

	public void setNumMaxClusters(int numMaxClusters) {
		this.numMaxClusters = numMaxClusters;
	}

	public int getMaximumInfeasibleSequenceLengthCheck() {
		return maximumInfeasibleSequenceLengthCheck;
	}

	public void setMaximumInfeasibleSequenceLengthCheck(
			int maximumInfeasibleSequenceLengthCheck) {
		this.maximumInfeasibleSequenceLengthCheck = maximumInfeasibleSequenceLengthCheck;
	}

	public boolean isMergeWhenTailsMatch() {
		return mergeWhenTailsMatch;
	}

	public void setMergeWhenTailsMatch(boolean mergeWhenTailsMatch) {
		this.mergeWhenTailsMatch = mergeWhenTailsMatch;
	}

	public int getMaximumGenerationsWithoutProgress() {
		return maximumGenerationsWithoutProgress;
	}

	public void setMaximumGenerationsWithoutProgress(
			int maximumGenerationsWithoutProgress) {
		this.maximumGenerationsWithoutProgress = maximumGenerationsWithoutProgress;
	}
	
	private int maximumGenerationsWithoutProgress = 20;
	
	private int populationSize = 100;
	private int maximumGenerations = 100;
	private int numTournaments = (int)Math.floor(0.1 * this.populationSize);
	private double mutationProbability = -1;
	private boolean saveFinalPopulationToFile = true;
	private boolean useMultiObjectiveGA = false;
	private long randomNumberSeed = System.currentTimeMillis();

	private int numMaxClusters = 10;
	private int maximumInfeasibleSequenceLengthCheck = 7;
	
	private boolean mergeWhenTailsMatch = false;
}
