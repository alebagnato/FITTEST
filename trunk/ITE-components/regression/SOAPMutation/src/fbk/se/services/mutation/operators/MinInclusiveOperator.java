package fbk.se.services.mutation.operators;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.Facet;
import fbk.se.services.mutation.constraints.OnElementType;

/**
 * Mutate an input node of type Integer or Double based on 
 * its lower bound constraint.
 * 
 * @author cdnguyen
 *
 */
public class MinInclusiveOperator implements IMutationOperator {

	@Override
	public Node mutate(Node input, OnElementType constraint) {
		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null){
			return null;
		}
		
		String base = restriction.getBase().getLocalPart();
		
		List<Object> facets = restriction.getFacets();
		for (Object o : facets) {
			// Find the facet that specify the mim inclusive value
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement)o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType.equals(IMutationOperator.XSD_MUTATION_MININCLUSIVE)) {
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					Double min = null;
					if (base.equalsIgnoreCase("integer")
						 || base.equalsIgnoreCase("double")){
						// Integer or Double
						try {
							min = Double.valueOf(value);
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
					} else {
						// ignore
					}
					
					if (min != null){
						try {
							Node mutatedNode = input.cloneNode(true);
//							mutatedNode.setNodeValue(String.valueOf(min - 1));
							if (mutatedNode.getFirstChild() != null){
								mutatedNode.getFirstChild().setTextContent(String.valueOf((int)(min - 1)));
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
