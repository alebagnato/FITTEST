package eu.fittest.phplog.analyzer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;


public class Type extends Action
{

    private final String text;

    public Type(Element target, String text)
    {
        super(target);
        this.text = text;
    }
    
    @Override
    public String toString()
    {
        String id = getTarget().getAttributes().get("id");
        String reference = id != null ? "id="+id : "xpath="+pathToString(getTarget().getPath());
        return "type: " + reference + " : " + text;
    }
        
    @Override
    public void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        String pageText = getTarget().getAttributes().get("value");
        if(pageText == null) pageText = "";
        if(text != null && !text.equals(pageText))
        {
        xmlwriter.writeStartElement("E");
          xmlwriter.writeStartElement("O");
            xmlwriter.writeAttribute("ty", "Struct");
              writeField(xmlwriter, "action","string","type");
              xmlwriter.writeStartElement("fd");
                xmlwriter.writeAttribute("n", "parameters");      

                xmlwriter.writeStartElement("O");
                xmlwriter.writeAttribute("ty", "Array");
                  writeXPath(xmlwriter);  
                  writeID(xmlwriter);
                  writeField(xmlwriter, "text", "string", text);
               xmlwriter.writeEndElement();
             xmlwriter.writeEndElement();
           xmlwriter.writeEndElement();
        xmlwriter.writeEndElement();
        }
    }
}
