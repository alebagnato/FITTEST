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
import eu.fbk.se.selenium.renders.FlexElementRender;
import eu.fbk.se.utils.Validator;
import eu.fbk.se.webelement.FlexElementBean;

public class CTE2FlexSelenium extends AbstractTransformer {

	public CTE2FlexSelenium(AbstractTemplateProvider _templateProvider,
			String packageName, String className, String _targetPage,
			String seleniumDriver) throws TransformException {
		super(_templateProvider, packageName, className, _targetPage, seleniumDriver);
		
		templateProvider.registerRenderer(FlexElementBean.class, new FlexElementRender());
		
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

		List<FlexElementBean> elements = new ArrayList<FlexElementBean>();
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
							if (!e.getTargetEventToFire().equals("clickButton")){
								parameters = buildParameters(c, testMarkTrue.split(" "), e);
							}
							event = e.getTargetEventToFire();
						}
						id = e.getReachedById();
					}
				} else {
					parameters = buildParameters(c, testMarkTrue);
				}

				FlexElementBean elem = new FlexElementBean(id, event, parameters);
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

		List<FlexElementBean> elements = new ArrayList<FlexElementBean>();
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
						id = e.getReachedById();
					}
				} 

				FlexElementBean elem = new FlexElementBean(id, event, parameters);
				elements.add(elem);
			}
		}
		tcTemplate.add("FElements", elements);

		return tcTemplate.render();
	}

}
