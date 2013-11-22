package fbk.se.services.mutation.operators;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.Facet;
import fbk.se.services.mutation.constraints.OnElementType;
import fbk.se.services.mutation.utils.Generator;

/**
 * Mutate a string based on a constraint on length
 * 
 * @author cdnguyen
 *
 */
public class LengthOperator implements IMutationOperator {

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
				if (constraintType.equals(IMutationOperator.XSD_MUTATION_MAXLENGTH)) {
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					Integer length = null;
					if (base.equalsIgnoreCase("string")){
						// String only
						try {
							length = Integer.valueOf(value);
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
					} else {
						// ignore
					}
					
					if (length != null){
						try {
							Node mutatedNode = input.cloneNode(true);
							Node firstChild = mutatedNode.getFirstChild();
							if (firstChild != null){
								String currentString = firstChild.getTextContent();
								currentString.concat(Generator.generateString(length));
								firstChild.setTextContent(currentString);
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
