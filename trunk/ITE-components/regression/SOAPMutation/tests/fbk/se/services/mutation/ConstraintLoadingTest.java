package fbk.se.services.mutation;

import java.io.File;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.junit.Test;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.Constraints;
import fbk.se.services.mutation.constraints.OnElementType;
import fbk.se.services.mutation.constraints.Pattern;
import fbk.se.services.mutation.constraints.TotalDigits;
import fbk.se.services.mutation.constraints.WhiteSpace;

public class ConstraintLoadingTest {

	@Test
	public void loadConstraintTest() {
		JAXBContext jct;
		try {
			jct = JAXBContext
					.newInstance("fbk.se.services.mutation.constraints");
			Unmarshaller um = jct.createUnmarshaller();

			String fileName = "xsd/EbayFindingAPIConstraints.xml";
			Constraints constraints = (Constraints) um.unmarshal(new File(
					fileName));

			for (OnElementType element : constraints.getOnElement()) {
				System.out.println(element.getXpath());
				ComplexRestrictionType restriction = element.getRestriction();
				List<Object> facets = restriction.getFacets();
				System.out.println(restriction.getBase().getLocalPart());
				for (Object o : facets) {
					if (o instanceof JAXBElement) {
						System.out.println(((JAXBElement) o).getName()
								.getLocalPart());
						System.out.println(((JAXBElement) o).getValue());
					} else if (o instanceof Pattern) {
						Pattern p = (Pattern) o;
						System.out.println("Pattern = " + p.getValue());
					} else if (o instanceof WhiteSpace) {
						WhiteSpace p = (WhiteSpace) o;
						System.out.println("WhiteSpace = " + p.getValue());
					} else if (o instanceof TotalDigits) {
						TotalDigits p = (TotalDigits) o;
						System.out.println("TotalDigits = " + p.getValue());
					}
				}
			}
			
			// XXX Inspect the output

		} catch (JAXBException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
