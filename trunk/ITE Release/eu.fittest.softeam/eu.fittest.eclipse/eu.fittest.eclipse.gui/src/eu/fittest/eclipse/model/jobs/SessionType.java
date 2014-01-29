package eu.fittest.eclipse.model.jobs;

public enum SessionType {
	RecordingSession("recording"), ReplayingSession("replaying"),OptimizationSession("optimization");
	
	private String _name;
	private SessionType(String name){
		_name = name;
	}
	
	public String getName(){
		return _name;
	}
}
