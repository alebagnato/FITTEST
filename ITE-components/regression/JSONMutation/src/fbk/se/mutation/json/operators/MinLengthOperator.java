package fbk.se.mutation.json.operators;

import java.util.List;
import java.util.Random;

import javax.xml.bind.JAXBElement;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import fbk.se.mutation.constraints.ComplexRestrictionType;
import fbk.se.mutation.constraints.Facet;
import fbk.se.mutation.constraints.OnElementType;
import fbk.se.mutation.utils.Generator;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

public class MinLengthOperator implements IMutationOperator {

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
			if (o instanceof JAXBElement) {
				JAXBElement tmp = (JAXBElement) o;
				String constraintType = tmp.getName().getLocalPart();
				if (constraintType
						.equals(IMutationOperator.XSD_MUTATION_MINLENGTH)) {
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					Integer minLength = null;
					if (base.equalsIgnoreCase("string")) {
						// String only
						try {
							minLength = Integer.valueOf(value);
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
					} else {
						// ignore
					}

					if (minLength != null && minLength > 0) {
						JsonNode mutatedNode = input.deepCopy();
						// randomly choosing either to remove the field or to
						// return a shorter string
						Random ran = new Random();
						if (ran.nextDouble() < 0.1) {
							((ObjectNode) mutatedNode).remove(fieldName);
						} else {
							((ObjectNode) mutatedNode).put(fieldName,
									Generator.generateString(minLength - 1));
						}
					}
				}
			}
		}

		return null;
	}

}
