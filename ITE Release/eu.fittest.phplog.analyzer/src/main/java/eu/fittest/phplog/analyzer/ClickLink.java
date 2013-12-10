package eu.fittest.phplog.analyzer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;



public class ClickLink extends Action
{

    public ClickLink(Element target)
    {
        super(target);
    }

    @Override
    public String toString()
    {
        return "clickAndWait: link=exact: "+getTarget().getAttributes().get("href");
    }
    @Override
    public void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        xmlwriter.writeStartElement("E");
        xmlwriter.writeStartElement("O");
          xmlwriter.writeAttribute("ty", "Struct");
            writeField(xmlwriter, "action","string","clicklink");
            xmlwriter.writeStartElement("fd");
              xmlwriter.writeAttribute("n", "parameters");      

              xmlwriter.writeStartElement("O");
              xmlwriter.writeAttribute("ty", "Array");
                writeXPath(xmlwriter);  
                writeID(xmlwriter);
                writeField(xmlwriter, "url", "url", getTarget().getAttributes().get("href"));                
             xmlwriter.writeEndElement();
           xmlwriter.writeEndElement();
         xmlwriter.writeEndElement();
       xmlwriter.writeEndElement();    	
    }
    
}
