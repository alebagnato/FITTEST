package eu.fbk.se.transform;

import org.stringtemplate.v4.ST;

public class SeleniumDriverTemplateProvider extends AbstractTemplateProvider {

	public SeleniumDriverTemplateProvider(String templateFile) {
		super(templateFile);
	}

	@Override
	public ST getHeaderTemplate() {
		if (templateReady){
			return stgGroup.getInstanceOf("TestHeader");
		}
		return null;
	}

	@Override
	public ST getFooterTemplate() {
		if (templateReady){
			return stgGroup.getInstanceOf("TestFooter");
		}
		return null;
	}

	@Override
	public ST getTCTemplate() {
		if (templateReady){
			return stgGroup.getInstanceOf("TestCase");
		}
		return null;
	}

}
