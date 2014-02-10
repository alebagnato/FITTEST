package eu.fittest.phplog.eventabstraction;

import java.util.Map;
import java.util.Map.Entry;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public class VArray implements Value
{

    private final Map<String, Value> data;

    public VArray(Map<String, Value> data)
    {
        this.data = data;
    }

    @Override
    public void writeXML(XMLStreamWriter xmlStreamWriter)
            throws XMLStreamException
    {
        xmlStreamWriter.writeStartElement("O");
        
        xmlStreamWriter.writeAttribute("ty", "Array");
        for (Entry<String, Value> entry : data.entrySet())
        {
            xmlStreamWriter.writeStartElement("fd");
            xmlStreamWriter.writeAttribute("n", entry.getKey());
            entry.getValue().writeXML(xmlStreamWriter);
            xmlStreamWriter.writeEndElement();
        }
        xmlStreamWriter.writeEndElement();        
    }
    
    @Override
    public String toString()
    {
        return data.toString();
    }

    public Map<String, Value> getEntries()
    {
        return data;
    }
}
