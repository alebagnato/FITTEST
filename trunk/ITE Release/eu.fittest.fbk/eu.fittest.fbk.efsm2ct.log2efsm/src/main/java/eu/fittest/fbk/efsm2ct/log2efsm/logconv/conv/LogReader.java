package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Instance;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.xml.BaseXmlVisitor;

/**
 * Reads the content of a log file in UU XML format
 * 
 * 
 * @author tiella
 * 
 */

public class LogReader {

	public static class BodyVisitor extends BaseXmlVisitor {

		List<Event> events = new ArrayList<Event>();

		@Override
		public void visitElement(Element element) {

			NodeList list = element.getChildNodes();

			for (int i = 0; i < list.getLength(); i++) {

				Node child = list.item(i);

				if (child.getNodeName().equals("E")) {

					Element cElem = (Element) list.item(i);

					EventVisitor eventVisitor = new EventVisitor();

					eventVisitor.visitElement(cElem);

					events.add(eventVisitor.getEvent());

				}

			}

		}

		public List<Event> getEvents() {
			return events;
		}

	}

	public static class EventVisitor extends BaseXmlVisitor {

		private Event event;

		@Override
		public void visitElement(Element element) {

			event = new Event();

			// System.out.println("t=" + element.getAttribute("t"));

			ObjectVisitor eV = new ObjectVisitor();

			eV.visitElement((Element) element.getChildNodes().item(1));

			event.setRecordEvent(eV.getObject());

			ObjectVisitor aV = new ObjectVisitor();

			aV.visitElement((Element) element.getChildNodes().item(3));

			event.setAppAbstractState(aV.getObject());

		}

		public Event getEvent() {

			return event;
		}

	}

	public static class ObjectVisitor extends BaseXmlVisitor {

		Instance obj;

		@Override
		public void visitElement(Element element) {

			if (!element.getNodeName().equals("O")) {

				throw new IllegalArgumentException(element.toString());

			}

			obj = new Instance();

			obj.setType(element.getAttribute("ty").toString());

			// handle the special case of Arrays

			if (obj.getType().equals("Array")) {

				ArrayList<String> values = new ArrayList<String>();
				TypedValue typedValue = new TypedValue(values, "Object");
				obj.put("elem", typedValue);

			}

			// System.out.println("type:" + type);

			NodeList list = element.getChildNodes();

			for (int i = 0; i < list.getLength(); i++) {

				String nodeName = list.item(i).getNodeName();

				if (nodeName.equals("fd")) {

					FieldVisitor fV = new FieldVisitor();

					Element e = (Element) list.item(i);

					fV.visitElement(e);

					if (obj.getType().equals("Array") && fV.name.equals("elem")) {

						TypedValue typedValue = obj.get(fV.name);
						ArrayList<String> values = (ArrayList<String>) typedValue.getValue();
						values.add((String) fV.getValue());

					} else {
						TypedValue typedValue = new TypedValue(fV.getValue(), fV.getType());
						obj.put(fV.name, typedValue);
					}

				}

			}

		}

		public Instance getObject() {

			return obj;
		}

		public Instance getObj() {
			return obj;
		}

		public void setObj(Instance obj) {
			this.obj = obj;
		}

	}

	public static class FieldVisitor extends BaseXmlVisitor {

		String name;
		String type;
		Object value;

		@Override
		public void visitElement(Element element) {

			if (element.getNodeName().equals("fd")) {

				name = element.getAttribute("n").toString();

				Element value = (Element) element.getChildNodes().item(1);

				if (value.getNodeName().equals("O")) {

					ObjectVisitor oV = new ObjectVisitor();

					oV.visitElement(value);

					this.value = oV.getObj();
					this.type = oV.getObj().getType();

				} else if (value.getNodeName().equals("V")) {

					BasicTypeVisitor oV = new BasicTypeVisitor();

					oV.visitElement(value);

					type = oV.getType();
					this.value = oV.getValue();

				} else {
					System.out.println("WARNING: FieldVisitor, invalid content:" + element.getNodeName());
				}

			} else {
				System.out.println("FieldVisitor, I don't know that to do on:" + element.getNodeName());
			}

		}

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getType() {
			return type;
		}

		public void setType(String type) {
			this.type = type;
		}

		public Object getValue() {
			return value;
		}

		public void setValue(Object value) {
			this.value = value;
		}

		// @Override
		// public void visitAttr(Attr attr) {
		//
		// String name;
		//
		// if (attr.getName().equals("n")) {
		//
		// name = attr.toString();
		// System.out.println("attr name:" + name);
		//
		// }
		//
		// }

	}

	public static class BasicTypeVisitor extends BaseXmlVisitor {

		private String value;
		private String type;

		@Override
		public void visitElement(Element element) {

			// <V v="1" ty="ID" />

			if (element.getNodeName().equals("V")) {

				String tmp = element.getAttribute("v");
				value = sanitize(tmp);
				type = element.getAttribute("ty");

				// System.out.println("v=" + value + " type=" + type);
			} else {
				throw new IllegalArgumentException("element name:" + element.getNodeName() + " is not 'V'");
			}

		}

		private String sanitize(String tmp) {
			
			String res = tmp.replace(' ','_').replaceAll("[()]", "");
			
			return res;
		}

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}

		public String getType() {
			return type;
		}

		public void setType(String type) {
			this.type = type;
		}

		// @Override
		// public void visitAttr(Attr attr) {
		// // TODO Auto-generated method stub
		//
		// }

	}

	/**
	 * reads a log file in UU XML format
	 * 
	 * @param fXmlFile
	 * @throws ConverterException
	 */

	public List<Event> read(File fXmlFile) throws ConverterException {

		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dBuilder = dbFactory.newDocumentBuilder();
			Document doc = dBuilder.parse(fXmlFile);
			doc.getDocumentElement().normalize();

			Element root = (Element) doc.getDocumentElement();

			BodyVisitor bodyVisitor = new BodyVisitor();

			bodyVisitor.visitElement(root);

			List<Event> events = bodyVisitor.getEvents();

			return events;

		} catch (Exception e) {
			throw new ConverterException("Can't read file:" + fXmlFile, e);
		}

	}

}
