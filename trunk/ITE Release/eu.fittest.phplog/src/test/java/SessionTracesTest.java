import java.io.File;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.junit.Assert;
import org.junit.Test;
import org.xml.sax.SAXException;

import eu.fittest.phplog.PHPSESSIDSessionIdentifier;
import eu.fittest.phplog.PhpLogParser;
import eu.fittest.phplog.SessionTraces;


public class SessionTracesTest
{
    private static final FilenameFilter XMLFILESFILTER = new FilenameFilter()
    {
        
        @Override
        public boolean accept(File dir, String name)
        {
           
            return name.endsWith(".xml");
        }
    };

   @Test
    public void testSessionTraces() throws IOException, SAXException, JAXBException
    {
        SessionTraces<String> sessionTraces = new SessionTraces<String>(new PHPSESSIDSessionIdentifier(), 900);
        File[] pages = new File("../eu.fittest.phplog.clustering/src/test/resources").listFiles(XMLFILESFILTER);
        List<List<File>> result = sessionTraces.getSessionTracesFor(Arrays.asList(pages));
        System.out.println(result);
        Assert.assertEquals(3, result.size());
    }
    
    //@Test
    public void testBla() throws JAXBException, IOException
    {
        
        File[] pages = new File("src/test/resources").listFiles(XMLFILESFILTER);
        for (File file : pages)
        {
            
            FileWriter writer = new FileWriter(new File(file.getParentFile(),file.getName()+ ".html"));
            byte[] data = PhpLogParser.readLogFile(file).getData();
            String str = data == null ? "" : new String(data);
            writer.append(str);
            writer.close();
        }
    }
}
