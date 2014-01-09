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
import eu.fbk.se.fsm.cte.TestGroup.TestCase;
import eu.fbk.se.fsm.xinput.Event;
import eu.fbk.se.selenium.renders.FlashElementRender;
import eu.fbk.se.utils.Validator;
import eu.fbk.se.webelement.FlashElementBean;
import eu.fbk.se.webelement.FlexElementBean;

public class CTE2FlashSelenium extends AbstractTransformer {

	public CTE2FlashSelenium(AbstractTemplateProvider _templateProvider,
			String packageName, String className, String _targetPage,
			String seleniumDriver) throws TransformException {
		super(_templateProvider, packageName, className, _targetPage, seleniumDriver);
		
		templateProvider.registerRenderer(FlashElementBean.class, new FlashElementRender());
		
		header.add("WebDriver", seleniumDriver);
		footer.add("WebDriver", seleniumDriver);
	}

	@Override
	protected String buildTestCase(TestCase tc, Composition testSequence) {
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
//		tcTemplate.add("PageName", targetPage);

		List<FlashElementBean> elements = new ArrayList<FlashElementBean>();
		List<Object> steps = testSequence.getCompositionOrClassification();
		for (Object step : steps) {
			if (step instanceof Composition) {
				Composition c = (Composition) step;

				int index = c.getName().indexOf("_");
				String inferredEvent = (index < c.getName().length() - 1) ? c
						.getName().substring(index + 1) : c.getName();

				String parameters = "";
				String event = inferredEvent;
				String id = c.getName();
				if (domainInputs != null) {
					Event e = getDomainEvent(inferredEvent);
					if (e != null) {
						if (!Validator.isEmpty(e.getTargetEventToFire())) {
							event = e.getTargetEventToFire();
						}
//							if (!e.getWebEventToFire().equals("clickButton")){
						parameters = buildParameters(c, testMarkTrue.split(" "), e);
//							}
						id = e.getReachedById();
					} else {
						// temporary fix
						String[] tmp = c.getName().split("_");
						if (tmp.length == 3){
							id = tmp[1];
							event = tmp[2];
						}
					}
				} else {
					// temporary fix
					String[] tmp = c.getName().split("_");
					if (tmp.length == 3){
						id = tmp[1];
						event = tmp[2];
					}
					parameters = buildParameters(c, testMarkTrue);
				}
				
				String[] params = parameters.split(", ");

				FlashElementBean elem = new FlashElementBean(id, event, params);
				elements.add(elem);
			}
		}
		tcTemplate.add("FElements", elements);

		return tcTemplate.render();
	}

	@Override
	protected String buildDefaultTestCase(Composition testSequence) {
		ST tcTemplate = templateProvider.getTCTemplate();
		tcTemplate.add("TestDescription", "Default test case");
		tcTemplate.add("TestName", "testEventSequence");

		List<FlashElementBean> elements = new ArrayList<FlashElementBean>();
		List<Object> steps = testSequence.getCompositionOrClassification();
		for (Object step : steps) {
			if (step instanceof Composition) {
				Composition c = (Composition) step;

				int index = c.getName().indexOf("_");
				String inferredEvent = (index < c.getName().length() - 1) ? c
						.getName().substring(index + 1) : c.getName();

				String parameters = "";
				String event = inferredEvent;
				String id = c.getName();
				if (domainInputs != null) {
					Event e = getDomainEvent(inferredEvent);
					if (e != null) {
//						if (!Validator.isEmpty(e.getWebEventToFire())) {
							event = e.getTargetEventToFire();
//						}
						id = e.getReachedById();
					} else {
						// temporary fix
						String[] tmp = c.getName().split("_");
						if (tmp.length == 3){
							id = tmp[1];
							event = tmp[2];
						}
					}
				} else {
					// temporary fix
					String[] tmp = c.getName().split("_");
					if (tmp.length == 3){
						id = tmp[1];
						event = tmp[2];
					}
				} 
				
				String[] params = parameters.split(", ");

				FlashElementBean elem = new FlashElementBean(id, event, params);
				elements.add(elem);
			}
		}
		tcTemplate.add("FElements", elements);

		return tcTemplate.render();
	}

}
