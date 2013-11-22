package fbk.se.mutation.json.operators;

import java.util.List;

import javax.xml.bind.JAXBElement;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import fbk.se.mutation.constraints.ComplexRestrictionType;
import fbk.se.mutation.constraints.Facet;
import fbk.se.mutation.constraints.OnElementType;
import fbk.se.mutation.utils.Generator;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

public class LengthOperator implements IMutationOperator {

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
						.equals(IMutationOperator.XSD_MUTATION_MAXLENGTH)) {
					Facet valueFacet = (Facet) tmp.getValue();
					String value = valueFacet.getValue();
					Integer length = null;
					if (base.equalsIgnoreCase("string")) {
						// String only
						try {
							length = Integer.valueOf(value);
						} catch (NumberFormatException e) {
							e.printStackTrace();
						}
					} else {
						// ignore
					}

					if (length != null) {
						JsonNode mutatedNode = input.deepCopy();

						String currentContent = input.get(fieldName).asText();
						currentContent.concat(Generator.generateString(length));

						((ObjectNode) mutatedNode).put(fieldName,
								currentContent);

						// mutatedNode.setNodeValue(newEnum);
						return mutatedNode;
					}
				}
			}
		}

		return null;
	}

}
