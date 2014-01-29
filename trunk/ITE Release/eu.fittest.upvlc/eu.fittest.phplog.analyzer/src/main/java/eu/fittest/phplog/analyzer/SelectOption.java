package eu.fittest.phplog.analyzer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;


public class SelectOption extends Action
{

    private final Option option;

    public SelectOption(Element target, Option option)
    {
        super(target);
        this.option = option;
    }
    
    @Override
    public String toString()
    {
        String id = getTarget().getAttributes().get("id");
        String reference = id != null ? "id="+id : "xpath="+pathToString(getTarget().getPath());

        return "select: " + reference + " label: " + option.getAttributes().get("value");
    }
    @Override
    public void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        xmlwriter.writeStartElement("E");
        xmlwriter.writeStartElement("O");
          xmlwriter.writeAttribute("ty", "Struct");
            writeField(xmlwriter, "action","string","select");
            xmlwriter.writeStartElement("fd");
              xmlwriter.writeAttribute("n", "parameters");      

              xmlwriter.writeStartElement("O");
              xmlwriter.writeAttribute("ty", "Array");
                writeXPath(xmlwriter);  
                writeID(xmlwriter);
                writeField(xmlwriter, "label", "string", option.getAttributes().get("value"));
                writeField(xmlwriter, "option", "xpath", pathToString(option.getPath()));
             xmlwriter.writeEndElement();
           xmlwriter.writeEndElement();
         xmlwriter.writeEndElement();
       xmlwriter.writeEndElement();    	
    }

}
