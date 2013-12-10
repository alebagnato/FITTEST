package eu.fittest.eclipse.model.environment;

public class HostStates {
	public static final String NONE = "HOST_STATE_NONE";
	public static final String CONNECTED = "HOST_STATE_CONNECTED";
	public static final String INITIALIZED = "HOST_STATE_INITIALIZED";
	public static final String COMPONENT_READY = "HOST_STATE_COMPONENT_READY";
	public static final String LOGGING = "HOST_STATE_LOGGING";
	public static final String UPLOADING_STARTED = "HOST_STATE_UPLOADING_STARTED";
	public static final String UPLOADING_FAILED = "HOST_STATE_UPLOADING_FAILED";
	public static final String UPLOADING_FINISHED = "HOST_STATE_UPLOADING_FINISHED";
	public static final String EXECUTING = "HOST_STATE_EXECUTING";
	public static final String CLOSED = "HOST_STATE_CLOSED";
	
	public static final String[] ALL_POSSIBLE_STATES = {NONE, CONNECTED, INITIALIZED, COMPONENT_READY, 
		UPLOADING_STARTED, UPLOADING_FINISHED, UPLOADING_FAILED, 
		LOGGING, EXECUTING, CLOSED};

}
