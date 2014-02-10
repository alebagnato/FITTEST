package eu.fittest.phplog.analyzer;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import javax.xml.stream.XMLStreamException;

import org.xml.sax.SAXException;

public class Main
{
    public static void main(String[] args) throws IOException, SAXException,
            XMLStreamException
    {
        TraceAnalyzer analyzer = new TraceAnalyzer();
       
        for (String fname : args)
        {
            Reader inTrace = new FileReader(new File(fname));
            Writer outTrace = new FileWriter(new File(fname+".client.xml"));
            analyzer.analyzeTrace(inTrace, outTrace);
        }

    }
 }
