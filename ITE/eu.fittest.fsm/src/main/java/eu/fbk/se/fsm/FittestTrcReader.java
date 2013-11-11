package eu.fbk.se.fsm;

import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

public class FittestTrcReader implements TraceFileReader
{

    private XMLStreamReader xmlReader;

   
    public FittestTrcReader()
    {
    }
    
    @Override
    public void close() throws IOException
    {
        try
        {
            xmlReader.close();
        } catch (XMLStreamException e)
        {
            throw new IOException(e);
        }
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

    
    @Override
    public String nextEvent() throws IOException
    {
        String result = null;

        try
        {
            if(xmlReader.hasNext() &&  xmlReader.nextTag() == XMLStreamConstants.START_ELEMENT)
            {
                checkTag(xmlReader, "E", true);
                xmlReader.nextTag();
                checkTag(xmlReader, "O", true);
                xmlReader.nextTag();
                checkTag(xmlReader, "fd", true);
                xmlReader.nextTag();
                checkTag(xmlReader, "V", true);
                String url = xmlReader.getAttributeValue(null, "v");
                xmlReader.nextTag();
                checkTag(xmlReader, "V", false);
                xmlReader.nextTag();
                checkTag(xmlReader, "fd", false);

                Map<String,String> params = readXmlParameters(xmlReader);

                xmlReader.nextTag();
                checkTag(xmlReader, "O",false);
                xmlReader.nextTag();
                checkTag(xmlReader, "E",false);
                result = url+params.toString();
            }
        } catch (XMLStreamException e)
        {
            throw new IOException(e);
        }
        return result;
    }
    private static Map<String,String> readXmlParameters(XMLStreamReader xmlReader) throws XMLStreamException
    {
        Map<String,String> result = new HashMap<String, String>();
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", true);
        xmlReader.nextTag();
        checkTag(xmlReader, "O", true);
        while(xmlReader.nextTag() != XMLStreamConstants.END_ELEMENT )
        {
            String name, value;
            checkTag(xmlReader, "fd", true);
            name = xmlReader.getAttributeValue(null, "n");
            xmlReader.nextTag();
            checkTag(xmlReader, "V", true);
            value = xmlReader.getAttributeValue(null, "v");
            xmlReader.nextTag();
            checkTag(xmlReader, "V", false);
            xmlReader.nextTag();
            checkTag(xmlReader, "fd", false);
            result.put(name,value);
        }
        checkTag(xmlReader, "O", false);
        xmlReader.nextTag();
        checkTag(xmlReader, "fd", false); 
        return result;
    }

    @Override
    public void open(Reader reader) throws IOException
    {
        try
        {
            xmlReader = XMLInputFactory.newFactory().createXMLStreamReader(reader);
            xmlReader.nextTag();
            checkTag(xmlReader, "body", true);
        } catch (XMLStreamException e)
        {
            throw new IOException(e);
        }    
    }

}
