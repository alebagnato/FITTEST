package eu.fittest.fbk.efsm2ct.log2efsm.logconv.xml;

public abstract class BaseXmlVisitor implements XmlVisitor {

	public BaseXmlVisitor() {
		super();
	}

	// public void visitChildren(Node node) {
	//
	// NodeList list = node.getChildNodes();
	//
	// for (int i = 0; i < list.getLength(); i++) {
	//
	// this.visitNode(list.item(i));
	//
	// }
	//
	// }

	// public void visitNode(Node n) {
	//
	// if (n instanceof Element) {
	// visitElement((Element) n);
	// } else if (n instanceof Text) {
	// // visitElement((Text)n);
	// // skipping #text
	// // } else if (n instanceof Attr) {
	// // visitAttr((Attr) n);
	// } else {
	// throw new IllegalArgumentException("unable to manage:"
	// + classString(n.getClass()));
	// }
	//
	// }

	private String classString(Class class1) {
		StringBuilder b = new StringBuilder();

		// for (Class i : class1.getInterfaces()) {
		//
		// b.append(i.getName()).append(":");
		//
		// }

		b.append(class1.getName());

		return b.toString();
	}

}