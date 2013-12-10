package eu.fittest.phplog.eventabstraction;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;

import eu.fittest.eventabstraction.Eventabstraction;

public class LogFileAbstracter
{
    private Abstractor abstractor;
    private XMLInputFactory xmlInputFactory;
    private XMLOutputFactory xmlOutputFactory;

    public LogFileAbstracter(Eventabstraction abstractionSpec)
    {
        abstractor = new Abstractor(abstractionSpec);
        xmlOutputFactory = XMLOutputFactory.newFactory();
        xmlInputFactory = XMLInputFactory.newFactory();

    }
    
    public void convert(Reader input, Writer output) throws IOException, XMLStreamException
    {
        XMLStreamWriter xmlStreamWriter=null;
        XMLStreamReader xmlReader=null;
        try
        {

            xmlStreamWriter = xmlOutputFactory.createXMLStreamWriter(output);
            xmlStreamWriter.writeStartDocument("utf-8", "1.0");
            xmlStreamWriter.writeStartElement("body");
            xmlReader = xmlInputFactory.createXMLStreamReader(input);
          
            ConcreteLogParser logparser = new ConcreteLogParser(xmlReader,false);

            for (ConcreteEvent event : logparser)
            {       
                Event abstractEvent = abstractor.abstractEvent(event.url, event.get, event.post, event.cookie);
                Map<String, List<Parameter>> parameterMap = new HashMap<String, List<Parameter>>();
                List<Parameter> parameters = new ArrayList<Parameter>(abstractEvent.parameters.size());
                for (java.util.Map.Entry<String, String> parameter : abstractEvent.parameters.entrySet())
                {
                    parameters.add(new Parameter(parameter.getKey(), new VLiteral(parameter.getValue())));
                }
                parameterMap.put("parameters", parameters );
                LogFileConverter.writeXmlEvent(xmlStreamWriter, abstractEvent.url, parameterMap, null,null, null);
            }
            xmlStreamWriter.writeEndElement();
            xmlStreamWriter.writeEndDocument();
        }
        finally
        {
            if(xmlStreamWriter!=null) 
            {
                xmlStreamWriter.flush();
                xmlStreamWriter.close();
            }
            if(xmlReader !=null) 
            {
                xmlReader.close();
            }
        }
    }
    
}
