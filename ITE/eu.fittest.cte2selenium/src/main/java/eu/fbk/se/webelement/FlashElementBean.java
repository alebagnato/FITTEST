package eu.fbk.se.webelement;

public class FlashElementBean {
	private String id;
	private String event;
	private String[] params;

	
	public FlashElementBean(String id, String event, String[] params) {
		super();
		this.id = id;
		this.event = event;
		this.params = params;
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


	public String[] getParams() {
		return params;
	}


	public void setParams(String[] params) {
		this.params = params;
	}
	
	
}
