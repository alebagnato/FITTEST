package eu.fittest.phplog.analyzer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;


public class Check extends Action
{

    private final boolean check;

    public Check(Element target)
    {
        this(target, true);
    }
    public Check(Element target,boolean check)
    {
        super(target);
        this.check = check;
    }
   
    @Override
    public String toString()
    {   
        String id = getTarget().getAttributes().get("id");
        String reference = id != null ? "id="+id : "xpath="+pathToString(getTarget().getPath());
        return "checkbox: " + check + " : " + reference;
    }
    @Override
    public void writeXML(XMLStreamWriter xmlwriter) throws XMLStreamException
    {
        xmlwriter.writeStartElement("E");
        xmlwriter.writeStartElement("O");
          xmlwriter.writeAttribute("ty", "Struct");
            writeField(xmlwriter, "action","string",check? "check" : "uncheck");
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
