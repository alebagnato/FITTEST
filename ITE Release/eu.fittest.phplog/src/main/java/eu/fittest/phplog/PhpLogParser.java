package eu.fittest.phplog;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

public class PhpLogParser
{
    private static JAXBContext jaxbcontext;

    public static Phplog readLogFile(File file) throws JAXBException
    {
        JAXBContext context = getJaxbContext();
        JAXBElement<?> doc = (JAXBElement<?>)context.createUnmarshaller().unmarshal(file);
        return (Phplog)doc.getValue();
    }

    private static JAXBContext getJaxbContext() throws JAXBException
    {
        if(jaxbcontext == null)
        {
            String packageName = Phplog.class.getPackage().getName();
            jaxbcontext = JAXBContext.newInstance( packageName );       
        }
        return jaxbcontext;
    }

}
