package eu.fittest.eclipse.model.jobs;

import java.util.ArrayList;
import java.util.List;

public enum JobModel {
	INSTANCE;
	
	private List<TestingSessionJob> _jobs;
	
	public List<TestingSessionJob> getJobs(){
		return _jobs;
	}
	
	private JobModel() {
		_jobs = new ArrayList<TestingSessionJob>();
	}
	
	public void addJob(TestingSessionJob job){
		_jobs.add(job);
	}
	
	public void removeJob(TestingSessionJob job){
		_jobs.remove(job);
	}
	
}
