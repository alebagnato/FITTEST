/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fbk.se.transform;

import java.util.ArrayList;
import java.util.List;

import org.stringtemplate.v4.ST;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.Marks;
import eu.fbk.se.fsm.cte.TestGroup;
import eu.fbk.se.fsm.xinput.Event;
import eu.fbk.se.fsm.xinput.WebElementType;
import eu.fbk.se.selenium.renders.BasicWebElementRender;
import eu.fbk.se.utils.Validator;
import eu.fbk.se.webelement.WebElementBean;

public class CTE2Selenium extends AbstractTransformer {

	public CTE2Selenium(AbstractTemplateProvider _templateProvider,
			String packageName, String className, String _targetPage,
			String seleniumDriver) throws TransformException {
		super(_templateProvider, packageName, className, _targetPage,
				seleniumDriver);
		
//		templateProvider.registerRenderer(WebElementBean.class,
//				new WebElementRender());
		templateProvider.registerRenderer(WebElementBean.class,
				new BasicWebElementRender());
		
		header.add("WebDriver", seleniumDriver);
		footer.add("WebDriver", seleniumDriver);
	}

	/**
	 * Export test case to file
	 * 
	 * @author cunduy
	 * 
	 * @param tc
	 * @param testSequence
	 * @param dom
	 * @return
	 */
	@Override
	protected String buildTestCase(TestGroup.TestCase tc,
			Composition testSequence) {

		List<Marks> selectedLeaves = tc.getMarks();
		StringBuilder testMarks = new StringBuilder();
		for (Marks marks : selectedLeaves) {
			if (!marks.getTrue().isEmpty()) {
				testMarks.append(marks.getTrue());
			}
		}
		String testMarkTrue = testMarks.toString();

		ST tcTemplate = templateProvider.getTCTemplate();
		tcTemplate.add("TestDescription", testMarkTrue);
		tcTemplate.add("TestName", "test_" + tc.getId());
		tcTemplate.add("PageName", targetPage);

		List<WebElementBean> elements = new ArrayList<WebElementBean>();
		List<Object> steps = testSequence.getCompositionOrClassification();
		for (Object step : steps) {
			if (step instanceof Composition) {
				Composition c = (Composition) step;

				int index = c.getName().indexOf("_");
				String inferredEvent = (index < c.getName().length() - 1) ? c
						.getName().substring(index + 1) : c.getName();

				String parameters = "";
				String event = inferredEvent; // + "(" + parameters + ")";
				
				WebElementBean elem = new WebElementBean();
//				elem.setId(c.getName()); //default
				
				if (domainInputs != null) {
					Event e = getDomainEvent(inferredEvent);
					if (e != null) {
						// build params
						parameters = buildParameters(c, testMarkTrue.split(" "), e);
						
						if (!Validator.isEmpty(e.getTargetEventToFire())){
							event = e.getTargetEventToFire();
						} else {
							event = "click"; // default
						}
						
						elem.setId(e.getReachedById());
						elem.setName(e.getReachedByName());
						elem.setXpath(e.getReachedByXPath());
						
						elem.setLinkText(e.getReachedByLinkText());
						elem.setPartialLinkText(e.getReachedByPartialLinkText());
						elem.setTagName(e.getReachedByTagName());
						elem.setUrl(e.getReachedByURL());
						elem.setCss(e.getReachedByCSS());
						
						if (e.getWebType() != null){
							elem.setType(e.getWebType());
						} else {
							elem.setType(WebElementType.BUTTON); // default
						}
						
						// Dealing with app like cart when there are multiple 
						// controls have a same name, the xpath need to be adjusted 
						
						/*
						if (!e.getWebEventToFire().isEmpty()) {
							if (isBrowerStandardEvent(e.getWebEventToFire())) {
								xpath = adjustXPath(e, inferredEvent,
										parameters);
							} else {
								xpath = e.getReachedByXPath();
							}
							event = "click()"; // e.getWebEventToFire();
												// Selenium Driver Event Wrapper
						}
						*/
						
					}
				} else {
					parameters = buildParameters(c, testMarkTrue);
//					event = inferredEvent + "(" + parameters + ")";
				}

				elem.setParams(parameters);
				elem.setEvent(event);

				elements.add(elem);
			}
		}
		tcTemplate.add("WElements", elements);

		return tcTemplate.render();
	}

	/**
	 * check if an event is a standard browser event
	 * 
	 * @author cuduynguyen
	 * 
	 * @param webEventToFire
	 * @return
	 */
	private boolean isBrowerStandardEvent(String webEventToFire) {
		if (webEventToFire == null) {
			return false;
		}

		String event = webEventToFire.toLowerCase();
		if (event.startsWith("click") 
				|| event.startsWith("type")
				|| event.startsWith("select")){
			return true;
		}
//				|| event.startsWith("onload") || event.startsWith("onunload")
//				|| event.startsWith("onblur") || event.startsWith("onfocus")
//				|| event.startsWith("onreset") || event.startsWith("onselect")
//				|| event.startsWith("onsubmit")
//				|| event.startsWith("onkeydown") || event.startsWith("onkeyup")
//				|| event.startsWith("onkeypress")
//				|| event.startsWith("ondblclick")
//				|| event.startsWith("onclick")
//				|| event.startsWith("onmousedown")
//				|| event.startsWith("onmousemove")
//				|| event.startsWith("onmouseout")
//				|| event.startsWith("onmouseover")
//				|| event.startsWith("onmouseup")
//				|| event.startsWith("onchange")) {
//			return true;
//		}

		return false;
	}

	
	@Override
	protected String buildDefaultTestCase(Composition testSequence) {
		ST tcTemplate = templateProvider.getTCTemplate();
		tcTemplate.add("TestDescription", "Default test case");
		tcTemplate.add("TestName", "testEventSequence");
		tcTemplate.add("PageName", targetPage);

		List<WebElementBean> elements = new ArrayList<WebElementBean>();
		List<Object> steps = testSequence.getCompositionOrClassification();
		for (Object step : steps) {
			if (step instanceof Composition) {
				Composition c = (Composition) step;

				int index = c.getName().indexOf("_");
				String inferredEvent = (index < c.getName().length() - 1) ? c
						.getName().substring(index + 1) : c.getName();

				String parameters = "";
				String event = inferredEvent;
				
				WebElementBean elem = new WebElementBean();
				
				if (domainInputs != null) {
					Event e = getDomainEvent(inferredEvent);
					if (e != null) {
						
						parameters = buildParameters(c, e);
						
						if (!Validator.isEmpty(e.getTargetEventToFire())){
							event = e.getTargetEventToFire();
						} else {
							event = "click"; // default
						}
						
						elem.setId(e.getReachedById());
						elem.setName(e.getReachedByName());
						elem.setXpath(e.getReachedByXPath());
						elem.setLinkText(e.getReachedByLinkText());
						elem.setPartialLinkText(e.getReachedByPartialLinkText());
						elem.setTagName(e.getReachedByTagName());
						elem.setUrl(e.getReachedByURL());
						elem.setCss(e.getReachedByCSS());
						
						if (e.getWebType() != null){
							elem.setType(e.getWebType());
						} else {
							elem.setType(WebElementType.BUTTON); // default
						}
						
					}
				}

				elem.setParams(parameters);
				elem.setEvent(event);

				elements.add(elem);
			}
		}
		tcTemplate.add("WElements", elements);

		return tcTemplate.render();
	}

}
