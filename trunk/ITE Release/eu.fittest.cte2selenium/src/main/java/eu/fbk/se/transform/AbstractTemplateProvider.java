package eu.fbk.se.transform;

import org.stringtemplate.v4.AttributeRenderer;
import org.stringtemplate.v4.ST;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

public abstract class AbstractTemplateProvider {
	protected boolean templateReady;
	protected STGroup stgGroup;
	
	public AbstractTemplateProvider(String templateFile) {
		init(templateFile);
	}
	
	public abstract ST getHeaderTemplate();
	public abstract ST getFooterTemplate();
	public abstract ST getTCTemplate();

	/**
	 * Load template group files
	 * @param templateFile
	 */
	protected void init(String templateFile){
		try {
			stgGroup = new STGroupFile(templateFile);
			templateReady = true;
		} catch (Exception e){
			e.printStackTrace();
			templateReady = false;
		}
	}
	
	/**
	 * Register render
	 * @param clz
	 * @param render
	 */
	public void registerRenderer(Class clz, AttributeRenderer render){
		if (templateReady){
			stgGroup.registerRenderer(clz, render);
		}
	}

	public boolean isTemplateReady() {
		return templateReady;
	}
	
	
}
