package eu.fbk.se.transform;

import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.management.RuntimeErrorException;
import javax.xml.bind.JAXBElement;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.stringtemplate.v4.ST;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import eu.fbk.se.datagenerator.BooleanGenerator;
import eu.fbk.se.datagenerator.DataGeneratorFactory;
import eu.fbk.se.datagenerator.DateGenerator;
import eu.fbk.se.datagenerator.IDataGenerator;
import eu.fbk.se.datagenerator.StringGenerator;
import eu.fbk.se.fsm.cte.Classification;
import eu.fbk.se.fsm.cte.Composition;
import eu.fbk.se.fsm.cte.Marks;
import eu.fbk.se.fsm.cte.TestGroup;
import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.Event;
import eu.fbk.se.fsm.xinput.WebElementType;
import eu.fbk.se.fsm.xinput.Xinput;
import eu.fbk.se.selenium.renders.BasicWebElementRender;
import eu.fbk.se.utils.Constants;
import eu.fbk.se.utils.Validator;
import eu.fbk.se.webelement.HttpElementBean;
import eu.fbk.se.webelement.WebElementBean;

public class CTE2Http extends AbstractTransformer {

	private boolean debug;

	public CTE2Http(AbstractTemplateProvider _templateProvider,
			String packageName, String className, String _targetPage,
			String seleniumDriver) throws TransformException {

		super(_templateProvider, packageName, className, _targetPage,
				seleniumDriver);

		// templateProvider.registerRenderer(WebElementBean.class,
		// new WebElementRender());
		templateProvider.registerRenderer(WebElementBean.class,
				new BasicWebElementRender());

		header.add("WebDriver", seleniumDriver);
		footer.add("WebDriver", seleniumDriver);
	}

	/**
	 * Export test case to file
	 * 
	 * @author cunduy,tiella
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

		// tcTemplate.add("PageName", targetPage);

		List<HttpElementBean> elements = new ArrayList<HttpElementBean>();
		List<Object> steps = testSequence.getCompositionOrClassification();

		for (Object step : steps) {

			if (step instanceof Composition) {

				Composition c = (Composition) step;

				int index = c.getName().indexOf("_");

				String inferredEvent = (index < c.getName().length() - 1) ? c
						.getName().substring(index + 1) : c.getName();

				String parameters = "";
				String event = inferredEvent; // + "(" + parameters + ")";

				HttpElementBean elem = new HttpElementBean();
				// elem.setId(c.getName()); //default

				if (domainInputs != null) {

					Event e = getDomainEvent(inferredEvent);

					if (e != null) {
						// build params

						parameters = buildParameters(c,
								testMarkTrue.split(" "), e);

						if (debug) {
						System.out.println("parameters: " + parameters);
						}

						if (!Validator.isEmpty(e.getTargetEventToFire())) {
							event = e.getTargetEventToFire();
						} else {
							event = "GET"; // default
						}

						elem.setSequenceName(c.getName());
						
						// elem.setId(e.getReachedById());
						// elem.setName(e.getReachedByName());
						// elem.setXpath(e.getReachedByXPath());
						//
						// elem.setLinkText(e.getReachedByLinkText());
						// elem.setPartialLinkText(e.getReachedByPartialLinkText());
						// elem.setTagName(e.getReachedByTagName());
						elem.setUrl(e.getReachedByURL());
						// elem.setCss(e.getReachedByCSS());
						//
						// if (e.getWebType() != null){
						// elem.setType(e.getWebType());
						// } else {
						// elem.setType(WebElementType.BUTTON); // default
						// }

						// Dealing with app like cart when there are multiple
						// controls have a same name, the xpath need to be
						// adjusted

						/*
						 * if (!e.getWebEventToFire().isEmpty()) { if
						 * (isBrowerStandardEvent(e.getWebEventToFire())) {
						 * xpath = adjustXPath(e, inferredEvent, parameters); }
						 * else { xpath = e.getReachedByXPath(); } event =
						 * "click()"; // e.getWebEventToFire(); // Selenium
						 * Driver Event Wrapper }
						 */

					}
				} else {
					parameters = buildParameters(c, testMarkTrue);
					// event = inferredEvent + "(" + parameters + ")";
				}

				elem.setParams(parameters.split("&"));
				elem.setEvent(event);

				elements.add(elem);
			}
		}

		tcTemplate.add("WElements", elements);

		return tcTemplate.render();
	}

	// /**
	// * check if an event is a standard browser event
	// *
	// * @author cuduynguyen
	// *
	// * @param webEventToFire
	// * @return
	// */
	// private boolean isBrowerStandardEvent(String webEventToFire) {
	// if (webEventToFire == null) {
	// return false;
	// }
	//
	// String event = webEventToFire.toLowerCase();
	//
	// if (event.startsWith("click")
	// || event.startsWith("type")
	// || event.startsWith("select")){
	// return true;
	// }
	// // || event.startsWith("onload") || event.startsWith("onunload")
	// // || event.startsWith("onblur") || event.startsWith("onfocus")
	// // || event.startsWith("onreset") || event.startsWith("onselect")
	// // || event.startsWith("onsubmit")
	// // || event.startsWith("onkeydown") || event.startsWith("onkeyup")
	// // || event.startsWith("onkeypress")
	// // || event.startsWith("ondblclick")
	// // || event.startsWith("onclick")
	// // || event.startsWith("onmousedown")
	// // || event.startsWith("onmousemove")
	// // || event.startsWith("onmouseout")
	// // || event.startsWith("onmouseover")
	// // || event.startsWith("onmouseup")
	// // || event.startsWith("onchange")) {
	// // return true;
	// // }
	//
	// return false;
	// }

	@Override
	protected String buildDefaultTestCase(Composition testSequence) {
		ST tcTemplate = templateProvider.getTCTemplate();
		tcTemplate.add("TestDescription", "Default test case");
		tcTemplate.add("TestName", "testEventSequence");
		// tcTemplate.add("PageName", targetPage);

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

						if (!Validator.isEmpty(e.getTargetEventToFire())) {
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

						if (e.getWebType() != null) {
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

		// tcTemplate.add("WElements", elements);

		return tcTemplate.render();
	}

	/**
	 * Build detailed parameter with domain input - special case for an HTTP
	 * request also parameter names have to be returned
	 * 
	 * @param step
	 * @param testMarkTrue
	 * @param e
	 * @return
	 */
	@Override
	protected String buildParameters(Composition step, String[] testMarks,
			Event e) {
		if (e == null || domainInputs == null) {
			return "";
		}

		Xinput eventInput = getDomainInput(e);

		StringBuilder paramBuilder = new StringBuilder();
		for (Object coc : step.getCompositionOrClassification()) {
			if (coc instanceof Classification) {
				Classification classification = (Classification) coc;
				for (eu.fbk.se.fsm.cte.Class o : classification.getClazz()) {
					for (String mark : testMarks) {
						if (mark.equals(o.getId())) {

							// query domain input spec
							ComplexDataSpecType dataClz = queryDataClz(
									o.getName(), classification.getName(),
									eventInput);
							boolean paramOK = false;
							if (dataClz != null) {
								IDataGenerator generator = DataGeneratorFactory
										.createDataGenerator(dataClz);

								if (generator != null) {

									paramBuilder.append(classification.getName());
									
									paramBuilder.append("=");
									
									paramBuilder.append(generator
											.generate(dataClz));

									paramBuilder.append("&");
									paramOK = true;
								}
							}

							if (!paramOK) {
								
//								if (1 == 1) {
//									
////									System.err.println(o.getName()+", "+classification.getName()+", "+eventInput);
////									
////									dumpXmlDocument(convertToDOM(eventInput));
//									
//									throw new RuntimeException("shouldn't reach this point");
//								}
								
								// Check data type
								dataClz = queryDataClz(
										classification.getName(), eventInput);
								if ((dataClz != null)
										&& "string".equals(dataClz.getBase()
												.getLocalPart())) {
									paramBuilder.append(Constants.STRING_QUOTE);
								}
								paramBuilder.append(o.getName());

								if ((dataClz != null)
										&& "string".equals(dataClz.getBase()
												.getLocalPart())) {
									paramBuilder.append(Constants.STRING_QUOTE);
								}
								paramBuilder.append(", ");
							}

							break; // one mark is enough
						}
					}
				}
			}
			// TODO instantiate objects belonging to compositions
			/*
			 * else if (coc instanceof Composition) {
			 * paramBuilder.append(buildParameters((Composition) coc, testMarks,
			 * e)); }
			 */
		}
		if (paramBuilder.length() > 2) {
			return paramBuilder.substring(0, paramBuilder.length() - 1); // trim
																			// the
																			// last
																			// comma
		} else {
			return "";
		}
	}

	/**
	 * Populate parameters for event - special case for HTTP also the names of
	 * parameters has to be returned
	 * 
	 * @param c
	 * @param tc
	 * @return
	 */
	@Override
	protected String buildParameters(Composition step, String testMarkTrue) {
		StringBuilder paramBuilder = new StringBuilder();
		for (Object coc : step.getCompositionOrClassification()) {
			if (coc instanceof Classification) {
				for (eu.fbk.se.fsm.cte.Class o : ((Classification) coc)
						.getClazz()) {
					if (testMarkTrue.contains(o.getId())) {
						// paramBuilder.append(Constants.STRING_QUOTE);
						paramBuilder.append(o.getName());
						// paramBuilder.append(Constants.STRING_QUOTE);
						paramBuilder.append(", ");
					}
				}
			} else if (coc instanceof Composition) {
				paramBuilder.append(buildParameters((Composition) coc,
						testMarkTrue));
			}
		}
		if (paramBuilder.length() > 2) {
			return paramBuilder.substring(0, paramBuilder.length() - 1); // trim
																			// the
																			// last
																			// comma
		} else {
			return "";
		}
	}

}
