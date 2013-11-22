package fbk.se.mutation.json.operators;

import java.util.List;
import javax.xml.bind.JAXBElement;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import fbk.se.mutation.constraints.ComplexRestrictionType;
import fbk.se.mutation.constraints.NoFixedFacet;
import fbk.se.mutation.constraints.OnElementType;
import fbk.se.mutation.json.operators.IMutationOperator;
import fbk.se.mutation.utils.Generator;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

public class EnumerationOperator implements IMutationOperator {


	@Override
	public JsonNode mutate(JsonNode input, OnElementType constraint) {

		ComplexRestrictionType restriction = constraint.getRestriction();
		if (restriction == null) {
			return null;
		}
		
		// get the field name, last element of the path
		VerySimpleXpathParser pathParser = new VerySimpleXpathParser(constraint.getXpath());
		String fieldName = pathParser.getTargetFieldName();
		if (fieldName == null)
			return null;


		// calculate max-min length
		int min = Integer.MAX_VALUE;
		int max = Integer.MIN_VALUE;

		List<Object> facets = restriction.getFacets();
		for (Object o : facets) {
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

		JsonNode mutatedNode = input.deepCopy();
		((ObjectNode)mutatedNode).put(fieldName, newEnum);
		
		// mutatedNode.setNodeValue(newEnum);
		return mutatedNode;
	}
}
