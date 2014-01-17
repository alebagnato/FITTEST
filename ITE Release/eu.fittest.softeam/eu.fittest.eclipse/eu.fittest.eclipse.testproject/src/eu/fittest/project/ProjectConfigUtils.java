package eu.fittest.project;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import eu.fittest.project.config.TestProject;

public class ProjectConfigUtils {
	private static final String JAXB_PACKAGE = "eu.fittest.project.config";
	
	public static TestProject load(File f) throws JAXBException{
		JAXBContext jcontext = JAXBContext.newInstance(JAXB_PACKAGE);
		Unmarshaller um = jcontext.createUnmarshaller();
		TestProject project = (TestProject) um.unmarshal(f);
		return project;
	}

	public static void save(TestProject project, String file) throws JAXBException{
		JAXBContext jcontext = JAXBContext.newInstance(JAXB_PACKAGE);
		Marshaller marshaller = jcontext.createMarshaller();
		marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
		marshaller.marshal(project, new File(file));
	}
}
