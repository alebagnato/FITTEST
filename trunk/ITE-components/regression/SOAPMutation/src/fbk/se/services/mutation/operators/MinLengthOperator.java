package fbk.se.services.mutation.operators;

import java.util.List;
import java.util.Random;

import javax.xml.bind.JAXBElement;

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.Facet;
import fbk.se.services.mutation.constraints.OnElementType;
import fbk.se.services.mutation.utils.Generator;

/**
 * Mutate an input node of type String based on its min length 
 * constraint.
 * 
 * @author cdnguyen
 *
 */
public class MinLengthOperator implements IMutationOperator {

	@Override
	public Node mutate(Node input, OnElementType constraint) {
		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null){
			return null;
		} 
		
		String base = restriction.getBase().getLocalPart();
		
		List<Object> facets = restriction.getFacets();
		for (Object o : facets) {
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement)o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType.equals(IMutationOperator.XSD_MUTATION_MINLENGTH)) {
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					Integer minLength = null;
					if (base.equalsIgnoreCase("string")){
						// String only
						try {
							minLength = Integer.valueOf(value);
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
					} else {
						// ignore
					}
					
					if (minLength != null && minLength > 0){
						try {
							Node mutatedNode = input.cloneNode(true);
							//mutatedNode.setNodeValue(String.valueOf(min + 1));
							Node firstChild = mutatedNode.getFirstChild();
							if (firstChild != null){
								//String currentString = firstChild.getTextContent();
								//currentString.concat(Generator.generateString(minLength));
								
								// randomly choosing either to return null (10%) or a shorter string
								Random ran = new Random();
								if (ran.nextDouble() < 0.1 ){
									firstChild.setTextContent(null);
								} else {
									firstChild.setTextContent(Generator.generateString(minLength - 1));
								}
							}
							return mutatedNode;
						} catch (DOMException e) {
							e.printStackTrace();
							return null;
						}
					}
				}
			}
		}
		
		return null;
	}

}
