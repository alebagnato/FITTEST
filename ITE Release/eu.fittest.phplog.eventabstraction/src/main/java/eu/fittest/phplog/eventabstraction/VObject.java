package eu.fittest.phplog.eventabstraction;

import java.util.Map;
import java.util.Map.Entry;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

public class VObject implements Value
{

    private final String classname;
    private final Map<String, Value> fields;

    public VObject(String classname, Map<String, Value> fields)
    {
        this.classname = classname;
        this.fields = fields;
    }

    @Override
    public void writeXML(XMLStreamWriter xmlStreamWriter)
            throws XMLStreamException
    {
        xmlStreamWriter.writeStartElement("O");
        
        xmlStreamWriter.writeAttribute("ty", "Object");
        xmlStreamWriter.writeStartElement("fd");
        xmlStreamWriter.writeAttribute("n", "@classname");
        xmlStreamWriter.writeAttribute("v", classname);
        xmlStreamWriter.writeEndElement();
        for (Entry<String, Value> entry : fields.entrySet())
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
        return "object" + classname + fields;
    }
    
    public String getClassName()
    {
        return classname;
    }

    public Map<String, Value> getFields()
    {
        return fields;
    }
}
