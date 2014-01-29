package eu.fittest.phplog.eventabstraction;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import eu.fittest.eventabstraction.Eventabstraction;

public class ParseEventAbstraction
{

    private static JAXBContext jaxbcontext;

    public static Eventabstraction parseEventAbstractionFile(File file) throws JAXBException
    {
        JAXBContext context = getJaxbContext();
        JAXBElement<?> doc = (JAXBElement<?>) context.createUnmarshaller()
                .unmarshal(file);
        return (Eventabstraction) doc.getValue();
    }

    private static JAXBContext getJaxbContext() throws JAXBException
    {
        if (jaxbcontext == null)
        {
            String packageName = Eventabstraction.class.getPackage().getName();
            jaxbcontext = JAXBContext.newInstance(packageName);
        }
        return jaxbcontext;
    }

}
