package eu.fittest.phplog;

import java.io.File;

import javax.xml.bind.JAXBException;

import eu.fittest.phplog.Entry;
import eu.fittest.phplog.Phplog;

public class PHPSESSIDSessionIdentifier implements SessionIdentifier<String>
{

    private static final IPSessionIdentifier IPSESSIONIDENTIFIER = new IPSessionIdentifier();
    @Override
    public String getIdFor(File file)
    {
        String sessID = IPSESSIONIDENTIFIER.getIdFor(file);
        Phplog log;
        try
        {
            log = PhpLogParser.readLogFile(file);
            for (Entry cookie :  log.getRequest().getCookie().getEntry())
            {
                if("PHPSESSID".equals(cookie.getIndex()))
                {
                    sessID += "-" + cookie.getValue().toString();
                    break;
                }
            }
        } 
        catch (JAXBException e)
        {
            throw new RuntimeException(e);
        }
        return sessID;
    }
}
