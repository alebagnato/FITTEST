package fbk.se.mutation.json.operators;

import java.util.List;

import javax.xml.bind.JAXBElement;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import fbk.se.mutation.constraints.ComplexRestrictionType;
import fbk.se.mutation.constraints.Facet;
import fbk.se.mutation.constraints.OnElementType;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

public class MinInclusiveOperator implements IMutationOperator {

	@Override
	public JsonNode mutate(JsonNode input, OnElementType constraint) {
		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null) {
			return null;
		}

		// get the field name, last element of the path
		VerySimpleXpathParser pathParser = new VerySimpleXpathParser(
				constraint.getXpath());
		String fieldName = pathParser.getTargetFieldName();
		if (fieldName == null)
			return null;

		String base = restriction.getBase().getLocalPart();

		List<Object> facets = restriction.getFacets();
		for (Object o : facets) {
			// Find the facet that specify the mim inclusive value
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement) o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType
						.equals(IMutationOperator.XSD_MUTATION_MININCLUSIVE)) {
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					Double min = null;
					if (base.equalsIgnoreCase("integer")
							|| base.equalsIgnoreCase("double")) {
						// Integer or Double
						try {
							min = Double.valueOf(value);
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
					} else {
						// ignore
					}

					if (min != null) {
						JsonNode mutatedNode = input.deepCopy();
						int newVal = (int) (min - 1);
						((ObjectNode) mutatedNode).put(fieldName, newVal);
						return mutatedNode;
					}
				}
			}
		}

		return null;
	}

}
