package eu.fbk.se.datagenerator;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Random;

import javax.xml.bind.JAXBElement;

import eu.fbk.se.fsm.xinput.ComplexDataSpecType;
import eu.fbk.se.fsm.xinput.Facet;
import eu.fbk.se.utils.Constants;

public class DateGenerator implements IDataGenerator {
	private static Random ranGenerator = new Random();

	public String generate(ComplexDataSpecType dataClz) {
		DateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
		
		List<Object> facets =  dataClz.getFacets();
		
		Date min = new Date();
		Date max = new Date();
		try {
			min = formatter.parse("01/01/1900");
			max = formatter.parse("31/12/2999");
		} catch (ParseException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		boolean minPresent = false;
		boolean maxPresent = false;
		
		for (Object o : facets) {
			// Find the facet that specify the min/max inclusive value
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement)o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MINEXCLUSIVE)
						|| constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MININCLUSIVE)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						min = formatter.parse(value);
						minPresent = true;
					} catch (ParseException e) {
						e.printStackTrace();
					}
				}

				if (constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MAXEXCLUSIVE)
						|| constraintType.equals(IDataGenerator.DOMAIN_CONSTRAINT_MAXINCLUSIVE)) {
					// treat these two type equally
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					try {
						max = formatter.parse(value);
						maxPresent = true;
					} catch (ParseException e) {
						e.printStackTrace();
					}
				}
			}
			
			if (minPresent && maxPresent){
				long distance = max.getTime() - min.getTime();
				if (distance > 0){
					double d = ranGenerator.nextDouble();
					Date genDate = new Date(min.getTime() + (long)(d * (double)distance));
					return formatter.format(genDate);
				}
			}
		}
		
		long DELTA = 86398; // 2 days
		
		if (minPresent){
			Date genDate = new Date(min.getTime() + DELTA);
			return formatter.format(genDate);		
		} 
		
		if (maxPresent){
			Date genDate = new Date(max.getTime() - DELTA);
			return formatter.format(genDate);		 
		}
					
		Date currentDate = new Date();
		return formatter.format(currentDate);
	}

}
