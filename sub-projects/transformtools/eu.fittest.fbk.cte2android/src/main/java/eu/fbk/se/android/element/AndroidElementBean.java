package eu.fbk.se.android.element;

public class AndroidElementBean {
	private String id;
	private String event;
	private String[] params;
	
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
	
	public AndroidElementBean(String id, String event, String[] params) {
		super();
		this.id = id;
		this.event = event;
		this.params = params;
	}
	
	
}
