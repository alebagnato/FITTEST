package eu.fbk.se.webelement;

import eu.fbk.se.fsm.xinput.WebElementType;

public class WebElementBean {
//	public static final String IDENTIFIED_BY_NAME = "_name";
//	public static final String IDENTIFIED_BY_ID = "_id";
//	public static final String IDENTIFIED_BY_XPATH = "_xpath";
	
	String name;
	String id;
	String xpath;
	String tagName;
	String linkText;
	String partialLinkText;
	String url;
	String css;
	
	String event; // event to fire
	String modelEvent; // event originally called in the model
	String params;
	WebElementType type; // button, input, select

	public WebElementBean(String name, String xpath, String event) {
		super();
		this.name = name;
		this.xpath = xpath;
		this.event = event;
	}
	
	public WebElementBean(String id, String name, String path, String event) {
		this.id = id;
		this.name = name;
		this.xpath = path;
		this.event = event;
	}
	
	

	public WebElementBean(String name, String id, String xpath, String event,
			String params) {
		this.name = name;
		this.id = id;
		this.xpath = xpath;
		this.event = event;
		this.params = params;
	}

	public WebElementBean() {
		
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getXpath() {
		return xpath;
	}

	public void setXpath(String xpath) {
		this.xpath = xpath;
	}

	public String getEvent() {
		return event;
	}

	public void setEvent(String event) {
		this.event = event;
	}

	public String getParams() {
		return params;
	}

	public void setParams(String params) {
		this.params = params;
	}

	public WebElementType getType() {
		return type;
	}

	public void setType(WebElementType type) {
		this.type = type;
	}

	public String getTagName() {
		return tagName;
	}

	public void setTagName(String tagName) {
		this.tagName = tagName;
	}

	public String getLinkText() {
		return linkText;
	}

	public void setLinkText(String linkText) {
		this.linkText = linkText;
	}

	public String getPartialLinkText() {
		return partialLinkText;
	}

	public void setPartialLinkText(String partialLinkText) {
		this.partialLinkText = partialLinkText;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getCss() {
		return css;
	}

	public void setCss(String css) {
		this.css = css;
	}

	public String getModelEvent() {
		return modelEvent;
	}

	public void setModelEvent(String modelEvent) {
		this.modelEvent = modelEvent;
	}

}
