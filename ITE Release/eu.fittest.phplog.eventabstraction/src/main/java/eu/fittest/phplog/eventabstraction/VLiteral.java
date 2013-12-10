package eu.fittest.phplog.eventabstraction;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public class VLiteral implements Value
{

    private final String literal;

    public VLiteral(String literal)
    {
        this.literal = literal;
    }

    @Override
    public void writeXML(XMLStreamWriter xmlStreamWriter) throws XMLStreamException
    {
        xmlStreamWriter.writeStartElement("V");
        xmlStreamWriter.writeAttribute("ty", "String");            
        xmlStreamWriter.writeAttribute("v", literal);
        xmlStreamWriter.writeEndElement();        
    }
    
    @Override
    public String toString()
    {
        return literal;
    }

}
