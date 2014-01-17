package eu.fbk.se.webelement;

public class FlexElementBean {
	public static final String IDENTIFIED_BY_NAME = "_name";
	public static final String IDENTIFIED_BY_ID = "_id";
	public static final String IDENTIFIED_BY_XPATH = "_xpath";
	
	String id;
	String event; // event to fire
	String params;


	public String getParams() {
		return params;
	}

	public void setParams(String params) {
		this.params = params;
	}

	public FlexElementBean(String id, String event, String params) {
		super();
		this.id = id;
		this.event = event;
		this.params = params;
	}

	public FlexElementBean() {
		
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getEvent() {
		return event;
	}

	public void setEvent(String event) {
		this.event = event;
	}

}
