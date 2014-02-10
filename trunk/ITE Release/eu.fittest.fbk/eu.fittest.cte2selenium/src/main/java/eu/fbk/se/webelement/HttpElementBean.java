package eu.fbk.se.webelement;

import eu.fbk.se.fsm.xinput.WebElementType;

/**
 * Originally cloned from WebElementBean
 * @author tiella
 *
 */

public class HttpElementBean {

	public static final String POST_METHOD="POST";
	public static final String GET_METHOD="GET";
	
	private String sequenceName; // id in the xinput file this event refers to
	
	private String name;
	private String id;
	private String xpath;
	private String tagName;
	private String linkText;
	private String partialLinkText;
	private String url;
	private String css;
	
	private String method = "<UNDEFINED>"; // "POST" or "GET"
	
	private String event; // event to fire
	private String modelEvent; // event originally called in the model
	
	private String[] params;
	
	private WebElementType type; // button, input, select

//	public HttpElementBean(String name, String method, String url) {
//		
//		this.name = name;
//		this.method = method;
//		this.url = url;
//		
//	}
	
//	public HttpElementBean(String name, String xpath, String event) {
//		super();
//		this.name = name;
//		this.xpath = xpath;
//		this.event = event;
//	}
//	
//	public HttpElementBean(String id, String name, String path, String event) {
//		this.id = id;
//		this.name = name;
//		this.xpath = path;
//		this.event = event;
//	}
//	
//	
//
//	public HttpElementBean(String name, String id, String xpath, String event,
//			String params) {
//		this.name = name;
//		this.id = id;
//		this.xpath = xpath;
//		this.event = event;
//		this.params = params;
//	}
	
	

	public HttpElementBean() {
	}
	
	public String getSequenceName() {
		return sequenceName;
	}

	public void setSequenceName(String xmlId) {
		this.sequenceName = xmlId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public String getMethod() {
		return method;
	}

	public void setMethod(String method) {
		this.method = method;
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

	public String [] getParams() {
		return params;
	}

	public void setParams(String [] params) {
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
