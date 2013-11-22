package fbk.se.services.mutation.operators;

import java.util.List;
import javax.xml.bind.JAXBElement;

import org.w3c.dom.DOMException;
import org.w3c.dom.Node;

import fbk.se.services.mutation.constraints.ComplexRestrictionType;
import fbk.se.services.mutation.constraints.NoFixedFacet;
import fbk.se.services.mutation.constraints.OnElementType;
import fbk.se.services.mutation.utils.Generator;

/**
 * Mutate the input node based on a constraint specified as enumerations
 * @author cdnguyen
 *
 */
public class EnumerationOperator implements IMutationOperator {

	private static String TOKENS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

	@Override
	public Node mutate(Node input, OnElementType constraint) {

		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null) {
			return null;
		}

		// String base = restriction.getBase().getLocalPart();
		// constraint facets

		// calculate max-min length
		int min = Integer.MAX_VALUE;
		int max = Integer.MIN_VALUE;

		List<Object> facets = restriction.getFacets();
		for (Object o : facets) {
			List<OnElementType> list = null;
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement) o;
				String constraintType = tmp.getName().getLocalPart();
				if (!constraintType
						.equals(IMutationOperator.XSD_MUTATION_ENUMERATION)) {
					return null; // wrong type
				}

				NoFixedFacet valueFacet = (NoFixedFacet) tmp.getValue();
				String value = valueFacet.getValue();
				if (value.length() > max)
					max = value.length();
				if (value.length() < min)
					min = value.length();

			} else {
				return null;
			}
		}

		int len = (int) (((double) min + (double) max) / 2);
		String newEnum = Generator.generateString(len);

		try {
			Node mutatedNode = input.cloneNode(true);
			if (mutatedNode.getFirstChild() != null) {
				mutatedNode.getFirstChild().setTextContent(newEnum);
			}
			// mutatedNode.setNodeValue(newEnum);
			return mutatedNode;
		} catch (DOMException e) {
			e.printStackTrace();
			return null;
		}
	}
}
