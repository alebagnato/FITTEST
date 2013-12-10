package eu.fittest.agent.ite.services.progress.spec;


import eu.fittest.common.core.service.ServiceEvent;


public class ProgressEvent extends ServiceEvent<ILocalProgressService> {
    
    private int _progress;
	private String _taskId;

	public String getTaskId() {
		return _taskId;
	}

	public int getProgress() {
		return _progress;
	}

	public ProgressEvent(final ILocalProgressService source, String taskId, int progress) {
        super(source);
        _taskId = taskId;
        _progress = progress;
    }

}
