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

import eu.fbk.se.android.element.AndroidElementBean;
import eu.fbk.se.android.renders.AndroidElementRender;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.Marks;
import eu.fbk.se.fsm.cte.TestGroup.TestCase;
import eu.fbk.se.fsm.xinput.Event;
import eu.fbk.se.utils.Validator;

public class CTE2RobotiumAndroid extends AbstractTransformer {

	public CTE2RobotiumAndroid(AbstractTemplateProvider templateProvider,
			String packageName, String className, String targetPackage,
			String targetActivity) throws TransformException {
		super(templateProvider, packageName, className, "", "");

		templateProvider.registerRenderer(AndroidElementBean.class,
				new AndroidElementRender());

		header.add("TargetPackage", targetPackage);
		header.add("TargetActivity", targetActivity);

		footer.add("TargetPackage", targetPackage);
		footer.add("TargetActivity", targetActivity);
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
		// tcTemplate.add("PageName", targetPage);

		List<AndroidElementBean> elements = new ArrayList<AndroidElementBean>();
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
						parameters = buildParameters(c,
								testMarkTrue.split(" "), e);
						if (e.getReachedById() != null)
							id = e.getReachedById();
						else if (e.getReachedByName() != null)
							id = e.getReachedByName();
						else
							id = "#";
					} else {
						// temporary fix
						String[] tmp = c.getName().split("_");
						if (tmp.length == 3) {
							id = tmp[1];
							event = tmp[2];
						}
					}
				} else {
					// temporary fix
					String[] tmp = c.getName().split("_");
					if (tmp.length == 3) {
						id = tmp[1];
						event = tmp[2];
					}
					parameters = buildParameters(c, testMarkTrue);
				}

				String[] params = parameters.split(", ");

				AndroidElementBean elem = new AndroidElementBean(id, event,
						params);
				elements.add(elem);
			}
		}
		tcTemplate.add("AndroidElements", elements);

		return tcTemplate.render();
	}

	@Override
	protected String buildDefaultTestCase(Composition testSequence) {
		ST tcTemplate = templateProvider.getTCTemplate();
		tcTemplate.add("TestDescription", "Default test case");
		tcTemplate.add("TestName", "testEventSequence");

		List<AndroidElementBean> elements = new ArrayList<AndroidElementBean>();
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
						parameters = buildParameters(c, e);
						
						event = e.getTargetEventToFire();
						if (e.getReachedById() != null)
							id = e.getReachedById();
						else if (e.getReachedByName() != null)
							id = e.getReachedByName();
					} else {
						String[] tmp = c.getName().split("_");
						if (tmp.length == 3) {
							id = tmp[1];
							event = tmp[2];
						}
					}
				} else {
					String[] tmp = c.getName().split("_");
					if (tmp.length == 3) {
						id = tmp[1];
						event = tmp[2];
					}
				}

				String[] params = parameters.split(", ");

				AndroidElementBean elem = new AndroidElementBean(id, event,
						params);
				elements.add(elem);
			}
		}
		tcTemplate.add("AndroidElements", elements);

		return tcTemplate.render();
	}

}
