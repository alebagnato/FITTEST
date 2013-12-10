package eu.fittest.phplog.analyzer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;



public class OpenURL extends Action
{

    private final String url;

    public OpenURL(String url)
    {
        super(null);
        this.url = url;
    }
    
    @Override
    public String toString()
    {
        return "open: " +url;
    }
    @Override
    public void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        xmlwriter.writeStartElement("E");
        xmlwriter.writeStartElement("O");
          xmlwriter.writeAttribute("ty", "Struct");
            writeField(xmlwriter, "action","string","open");
            xmlwriter.writeStartElement("fd");
              xmlwriter.writeAttribute("n", "parameters");      

              xmlwriter.writeStartElement("O");
              xmlwriter.writeAttribute("ty", "Array");
              writeField(xmlwriter, "url", "url", url);
             xmlwriter.writeEndElement();
           xmlwriter.writeEndElement();
         xmlwriter.writeEndElement();
       xmlwriter.writeEndElement();    	
    }
}
