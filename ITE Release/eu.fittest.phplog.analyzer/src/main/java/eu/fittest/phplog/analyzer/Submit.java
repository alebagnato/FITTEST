package eu.fittest.phplog.analyzer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;


public class Submit extends Action
{

    public Submit(Element target)
    {
        super(target);
    }

    @Override
    public String toString()
    {
        String id = getTarget().getAttributes().get("id");
        String reference = id != null ? "id="+id : "xpath="+pathToString(getTarget().getPath());
        return "clickAndWait: " + reference;
    }
    @Override
    public void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        xmlwriter.writeStartElement("E");
        xmlwriter.writeStartElement("O");
          xmlwriter.writeAttribute("ty", "Struct");
            writeField(xmlwriter, "action","string","submit");
            xmlwriter.writeStartElement("fd");
              xmlwriter.writeAttribute("n", "parameters");      

              xmlwriter.writeStartElement("O");
              xmlwriter.writeAttribute("ty", "Array");
                writeXPath(xmlwriter);  
                writeID(xmlwriter);
             xmlwriter.writeEndElement();
           xmlwriter.writeEndElement();
         xmlwriter.writeEndElement();
       xmlwriter.writeEndElement();    	
    }
}
