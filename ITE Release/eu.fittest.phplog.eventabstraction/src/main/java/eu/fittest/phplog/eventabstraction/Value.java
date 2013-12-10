package eu.fittest.phplog.eventabstraction;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public interface Value
{

    public void writeXML(XMLStreamWriter xmlStreamWriter) throws XMLStreamException;

}
