/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
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
