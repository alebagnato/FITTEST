package eu.fittest.phplog.analyzer;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Collections;
import java.util.List;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import nu.validator.htmlparser.common.XmlViolationPolicy;
import nu.validator.htmlparser.sax.HtmlParser;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import eu.fittest.phplog.eventabstraction.ConcreteEvent;
import eu.fittest.phplog.eventabstraction.ConcreteLogParser;

public class TraceAnalyzer
{
    private boolean isRedirect;
    private List<InteractiveElement> prevPage;

    public TraceAnalyzer() 
    {
    }
    
    public void analyzeTrace (Reader inTrace, Writer outTrace) throws XMLStreamException, IOException, SAXException
    {
    	reset();
        XMLStreamWriter xmlwriter = XMLOutputFactory.newFactory().createXMLStreamWriter(outTrace);
         
        
        try
        {
            xmlwriter.writeStartDocument();
            xmlwriter.writeStartElement("body");

            ConcreteLogParser logparser = new ConcreteLogParser(inTrace, true);

            for (ConcreteEvent concreteEvent : logparser)
            {
                List<Action> actions = analyze(concreteEvent);
        		writeActions(xmlwriter, actions);
            }
            xmlwriter.writeEndElement();
            xmlwriter.writeEndDocument();

        } finally
        {
            if (xmlwriter != null)
                xmlwriter.close();
        }

    }

    public void reset()
    {
        isRedirect = false;
        prevPage = null;   	
    }

	public List<Action> analyze(ConcreteEvent concreteEvent) throws XMLStreamException,
			IOException, SAXException 
	{
		
		List<Action> actions;
		if (isRedirect)
		{
		    // skip;
		    actions = Collections.emptyList();
		} else if (prevPage == null)
		{
		    actions = Collections.<Action> singletonList(new OpenURL(
		            concreteEvent.url));
		} else
		{
		    actions = match(concreteEvent);
		    if (actions == null)
		    {
		        actions = Collections
		                .<Action> singletonList(new OpenURL(
		                        concreteEvent.url));
		    }

		}

		byte[] data = concreteEvent.data;

		InputSource input = new InputSource(new ByteArrayInputStream(data));
		HtmlContentHandler handler = new HtmlContentHandler();
		HtmlParser parser = new HtmlParser(XmlViolationPolicy.ALLOW);
		parser.setContentHandler(handler);
		parser.parse(input);

		isRedirect = concreteEvent.isRedirect;
		if (!isRedirect)
		    prevPage = handler.getResult();
		return actions;
	}

    private static void writeActions(XMLStreamWriter xmlwriter,
            List<Action> actions) throws XMLStreamException
    {
        for (Action action : actions)
        {
            action.writeXML(xmlwriter);
        }

    }
    private List<Action> match(ConcreteEvent concreteEvent)
    {
        List<Action> result = null;

        if (prevPage != null)
        {
            for (InteractiveElement element : prevPage)
            {
                List<Action> actions = element.match(concreteEvent);
                if (actions != null)
                {
                    result = actions;
                    break;
                }
            }
        }
        return result;
    }

}
