package eu.fittest.phplog.analyzer;

import java.util.List;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;


public abstract class Action
{

    private Element target;

    public Action(Element element)
    {
        this.target = element;
    }

    public Element getTarget()
    {
        return target;
    }
    @Override
    public String toString()
    {
        return ""+target;
    }
    String pathToString(List<PathItem> path)
    {
        String xpath = "";
        for (PathItem item : path)
        {

            String base = item.tag + "[" +item.index+"]";
            if(item.id != null)
            {
                base += "[";
                if(item.id != null)
                {
                    base += "@id='"+item.id+"'";
                }

                base += "]";
            }
            if(item.id != null)
            {
                xpath = "//" + base;
            }
            else
            {
                xpath += "/" + base;
            }
        }
        return xpath;
    }

    public abstract void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException;

    protected void writeField(XMLStreamWriter xmlwriter, String name, String type, String value)
    throws XMLStreamException
    {
        xmlwriter.writeStartElement("fd");
        xmlwriter.writeAttribute("n", name);      
        xmlwriter.writeStartElement("V");
        xmlwriter.writeAttribute("ty", type);      
        xmlwriter.writeAttribute("v", value);      
        xmlwriter.writeEndElement();  
        xmlwriter.writeEndElement();
    }
    
    protected void writeXPath(XMLStreamWriter xmlwriter)
    throws XMLStreamException
    {
        writeField(xmlwriter, "xpath", "xpath", pathToString(getTarget().getPath()));
    }
    protected void writeID(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        String id = getTarget().getAttributes().get("id");
        if(id != null)
        {
            writeField(xmlwriter, "id", "ID", id);
        }
    }
}
