package eu.fittest.fbk.efsm2ct.log2efsm.logconv.xml;

import org.w3c.dom.Element;

public interface XmlVisitor {

	public abstract void visitElement(Element element);

	// public abstract void visitAttr(Attr attr);

}