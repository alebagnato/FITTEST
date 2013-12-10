package eu.fittest.phplog.eventabstraction;

import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.apache.commons.codec.binary.Base64;

public class ConcreteLogParser implements Iterator<ConcreteEvent>, Iterable<ConcreteEvent>
{
    private final XMLStreamReader xmlReader;

    private ConcreteEvent event;

    private final boolean withData;

    public ConcreteLogParser(Reader reader, boolean withData) throws XMLStreamException
    {        
        this(XMLInputFactory.newFactory().createXMLStreamReader(reader), withData);    
    }
    
    public ConcreteLogParser(XMLStreamReader xmlReader, boolean withData)
    {
        this.xmlReader = xmlReader;
        this.withData = withData;
        advance();
    }
    
    private static void checkTag(XMLStreamReader xmlReader, String expect, boolean open) throws XMLStreamException
    {
        String tag = xmlReader.getLocalName();
        if(!expect.equals(tag))
            throw new XMLStreamException("Unexpected tag: " + tag + " expecting: " + expect, xmlReader.getLocation());
        if(open != (xmlReader.getEventType() == XMLStreamConstants.START_ELEMENT))
            throw new XMLStreamException("Unexpected " + (open? "close":"open") 
                                        +" tag:  expecting: " + (open? "open":"close") + " tag"
                                        , xmlReader.getLocation());
    }

    private ConcreteEvent readEvent() throws XMLStreamException
    {
        String url;
        boolean isRedirect;
        List<Parameter> get = new ArrayList<Parameter>();
        List<Parameter> post = new ArrayList<Parameter>();
        List<Parameter> cookie = new ArrayList<Parameter>();

        checkTag(xmlReader, "E", true);
        xmlReader.nextTag();
        checkTag(xmlReader, "O", true);
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", true);
        xmlReader.nextTag();
        checkTag(xmlReader, "V", true);
        url = xmlReader.getAttributeValue(null, "v");
        xmlReader.nextTag();
        checkTag(xmlReader, "V", false);
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", false);
        xmlReader.nextTag();

        checkTag(xmlReader, "fd", true);
        xmlReader.nextTag();
        checkTag(xmlReader, "V", true);
        isRedirect = "true".equals(xmlReader.getAttributeValue(null, "v"));
        xmlReader.nextTag();
        checkTag(xmlReader, "V", false);
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", false);
        
        
        readXmlParameters(get, post, cookie);
        readXmlParameters(get, post, cookie);
        readXmlParameters(get, post, cookie);
        
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", true);
        xmlReader.nextTag();
        checkTag(xmlReader, "V", true);
        String mime = xmlReader.getAttributeValue(null, "ty");
        byte[] data = null;
        StringBuffer buffer = new StringBuffer();
        while(xmlReader.next() == XMLStreamConstants.CHARACTERS)
        {
        	buffer.append(xmlReader.getText());
        }
        if(withData)
        {
            data = Base64.decodeBase64(buffer.toString());               
        }
        if(!xmlReader.isStartElement() && !xmlReader.isEndElement())
            xmlReader.nextTag();

        checkTag(xmlReader, "V", false);
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", false);             
        xmlReader.nextTag();
        
        
        checkTag(xmlReader, "O",false);
        xmlReader.nextTag();
        checkTag(xmlReader, "E",false);

        
        return new ConcreteEvent(url, get, post, cookie,isRedirect,mime,data);
    }

    private void readXmlParameters(List<Parameter> get,
             List<Parameter> post, List<Parameter> cookie) throws XMLStreamException
    {
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", true);
        String arrayName = xmlReader.getAttributeValue(null, "n");
        List<Parameter> result;
        if("get".equals(arrayName))
            result = get;
        else if("post".equals(arrayName))
            result = post;
        else if("cookie".equals(arrayName))
            result = cookie;
        else
        {
            System.err.println(arrayName);
            result = new ArrayList<Parameter>();
        }
        xmlReader.nextTag();
        checkTag(xmlReader, "O", true);
        while(xmlReader.nextTag() != XMLStreamConstants.END_ELEMENT )
        {
            String name;
            Value value;
            checkTag(xmlReader, "fd", true);
            name = xmlReader.getAttributeValue(null, "n");
            xmlReader.nextTag();
            value = readXMLValue();
            checkTag(xmlReader, "fd", false);
            result.add(new Parameter(name,value));
        }
        checkTag(xmlReader, "O", false);
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", false); 
    }
    
    private Value readXMLValue() throws XMLStreamException
    {
        Value value;
        String tag = xmlReader.getLocalName();
        if("V".equals(tag))
        {
            checkTag(xmlReader, "V", true);
            value = new VLiteral(xmlReader.getAttributeValue(null, "v"));
            xmlReader.nextTag();
            checkTag(xmlReader, "V", false);
            xmlReader.nextTag();
        }
        else
        {
            Map<String,Value> data = new LinkedHashMap<String, Value>();
            checkTag(xmlReader, "O", true);
            String type = xmlReader.getAttributeValue(null, "ty");
            while(xmlReader.nextTag() != XMLStreamConstants.END_ELEMENT )
            {
                String name;
                Value value2;
                checkTag(xmlReader, "fd", true);
                name = xmlReader.getAttributeValue(null, "n");
                xmlReader.nextTag();
                value2 = readXMLValue();
                checkTag(xmlReader, "fd", false);
                data.put(name,value2);
            }
            checkTag(xmlReader, "O", false);
            xmlReader.nextTag();
            if(type == null || "Array".equals(type))
            {
                value = new VArray(data);
            }
            else
            {
                value = new VObject(type, data);
            }            
        }
        return value;
    }

    ConcreteEvent advance()
    {
        try
        {
            if(xmlReader.getEventType() == XMLStreamConstants.START_DOCUMENT)
            {
                xmlReader.nextTag();       
                checkTag(xmlReader, "body", true);
                xmlReader.nextTag();
            }
            if(xmlReader.isStartElement())
            {
                event = readEvent();
                xmlReader.nextTag();
            }
            else
            {
                checkTag(xmlReader, "body", false);  
                event = null;
            }
        } 
        catch (XMLStreamException e)
        {
            throw new RuntimeException(e);
        }
        return event;
    }
    
  
    @Override
    public boolean hasNext()
    {
       return event != null;
    }

    @Override
    public ConcreteEvent next()
    {
        ConcreteEvent event = this.event;
        advance();
        return event;
    }

    @Override
    public void remove()
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<ConcreteEvent> iterator()
    {
        return this;
    }

}
