package eu.fittest.phplog.eventabstraction;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.apache.commons.codec.binary.Base64;

import eu.fittest.phplog.ArrayValue;
import eu.fittest.phplog.Entry;
import eu.fittest.phplog.ObjectValue;
import eu.fittest.phplog.PhpLogParser;
import eu.fittest.phplog.Phplog;
import eu.fittest.phplog.Property;

public class LogFileConverter
{
    private XMLOutputFactory xmlOutputFactory;    

    public LogFileConverter()
    {
        xmlOutputFactory = XMLOutputFactory.newFactory();
    }
  
    public void convert(List<File> traces, Writer output) throws IOException, XMLStreamException  
    {        
        XMLStreamWriter xmlStreamWriter=null;
        try
        {

            xmlStreamWriter = xmlOutputFactory.createXMLStreamWriter(output);

            xmlStreamWriter.writeStartDocument("utf-8", "1.0");
            xmlStreamWriter.writeStartElement("body");
            for (File file : traces)
            {
            	
				try {
					Phplog log = PhpLogParser.readLogFile(file);
					String url = log.getRequest().getUri();
					List<Parameter> get = makeParameters(log.getRequest().getGet());
					List<Parameter> post = makeParameters(log.getRequest().getPost());
					List<Parameter> cookie = makeParameters(log.getRequest().getCookie());
					Map<String,List<Parameter>> map = new LinkedHashMap<String,List<Parameter>>();
					map.put("get", get);
					map.put("post", post);
					map.put("cookie", cookie);
					byte[] data = log.getData();
					if(data == null)
						data = new byte[0];
					List<String> respHeaders = log.getResponse().getHeaders().getHeader();
					String contentType = getContentType(respHeaders );
					boolean isRedirect = hasLocationHeader(respHeaders );
					writeXmlEvent(xmlStreamWriter, url, map, isRedirect, data ,contentType);
				} catch (JAXBException e) {
					System.out.println("Failed to convert file " + file.getName()); // for debugging
					e.printStackTrace();
				}


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
        }
    }


    private boolean hasLocationHeader(List<String> headers) 
    {
        boolean result = false;
        for (String header : headers)
        {
            if( header.length() >= 9 &&
            	"location:".equalsIgnoreCase(header.substring(0,9)))
            {
                result = true;
                break;
            }
        }
        return result;
	}

	private String getContentType(List<String> headers)
    {
        String result = "text/html";
        for (String header : headers)
        {
            if(header.matches("[Cc]ontent-[Tt]ype:.*"))
            {
                result = header.substring("Content-Type:".length()).trim();
                break;
            }
        }
        return result;
    }

    static void writeXmlEvent(XMLStreamWriter xmlStreamWriter, String url,
            Map<String,List<Parameter>> parameterMap,Boolean isRedirect, byte[] data, String mimeType)
            throws XMLStreamException
    {
        xmlStreamWriter.writeStartElement("E");
        xmlStreamWriter.writeStartElement("O");
        xmlStreamWriter.writeAttribute("ty", "Struct");

        xmlStreamWriter.writeStartElement("fd");
        xmlStreamWriter.writeAttribute("n", "url");
        xmlStreamWriter.writeStartElement("V");
        xmlStreamWriter.writeAttribute("ty", "String");
        xmlStreamWriter.writeAttribute("v", url);
        xmlStreamWriter.writeEndElement();
        xmlStreamWriter.writeEndElement();

        if(isRedirect != null)
        {
	        xmlStreamWriter.writeStartElement("fd");
	        xmlStreamWriter.writeAttribute("n", "isRedirect");
	        xmlStreamWriter.writeStartElement("V");
	        xmlStreamWriter.writeAttribute("ty", "boolean");
	        xmlStreamWriter.writeAttribute("v", ""+isRedirect);
	        xmlStreamWriter.writeEndElement();
	        xmlStreamWriter.writeEndElement();
        }
        
        for (java.util.Map.Entry<String, List<Parameter>> entry : parameterMap.entrySet())
        {
            writeXmlParameters(xmlStreamWriter, entry.getValue(), entry.getKey());            
        }

        if(data != null && mimeType!= null)
        {
            xmlStreamWriter.writeStartElement("fd");
            xmlStreamWriter.writeAttribute("n", "data");
            xmlStreamWriter.writeStartElement("V");
            xmlStreamWriter.writeAttribute("ty", mimeType);
            xmlStreamWriter.writeCharacters(Base64.encodeBase64String(data));
            xmlStreamWriter.writeEndElement();
            xmlStreamWriter.writeEndElement();
        }
        
        xmlStreamWriter.writeEndElement();   
        xmlStreamWriter.writeEndElement();
    }



   

    private static void writeXmlParameters(XMLStreamWriter xmlStreamWriter, List<Parameter> parameters, String name)
            throws XMLStreamException
    {
        xmlStreamWriter.writeStartElement("fd");
        xmlStreamWriter.writeAttribute("n", name);
        xmlStreamWriter.writeStartElement("O");
        
        xmlStreamWriter.writeAttribute("ty", "Array");
        for (Parameter parameter : parameters)
        {
            xmlStreamWriter.writeStartElement("fd");
            xmlStreamWriter.writeAttribute("n", parameter.name);
            parameter.value.writeXML(xmlStreamWriter);
            xmlStreamWriter.writeEndElement();
        }
        xmlStreamWriter.writeEndElement();
        xmlStreamWriter.writeEndElement();
    }

    
    private static List<Parameter> makeParameters(ArrayValue values)
    {
        List<Parameter> result = new ArrayList<Parameter>();
        for(Entry entry: values.getEntry())
        {
            result.add(new Parameter(entry.getIndex(), toObject(entry.getValue())));
        }
        return result;
    }

    private static Value toObject(Object value)
    {/*
        @XmlElement(name = "object", type = ObjectValue.class),
        @XmlElement(name = "array", type = ArrayValue.class),
        @XmlElement(name = "int", type = Integer.class),
        @XmlElement(name = "bool", type = Boolean.class),
        @XmlElement(name = "float", type = Float.class),
        @XmlElement(name = "string", type = String.class)
        */
        if(value instanceof ObjectValue)
        {
            return toObject((ObjectValue)value);
        }
        else if(value instanceof ArrayValue)
        {
            return toObject((ArrayValue)value);
        }
        else
        {
            return new VLiteral(value.toString());
        }
        
    }

    private static VArray toObject(ArrayValue value)
    {
        Map<String,Value> result = new LinkedHashMap<String, Value>();
        for (Entry entry : value.getEntry())
        {
            result.put(entry.getIndex(), toObject(entry.getValue()));
        }
        return new VArray(result);
    }

    private static VObject toObject(ObjectValue value)
    {
        Map<String,Value> result = new LinkedHashMap<String, Value>();

        for (Property entry : value.getProperty())
        {
            result.put(entry.getName(), toObject(entry.getValue()));
        }
        return new VObject(value.getClassname(),result);
    }

}
