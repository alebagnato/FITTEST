package fbk.se.services.mutation.operators;

/**
 * Factory class used to create mutation operators based on 
 * the input mutation type.
 * 
 * @author cdnguyen
 *
 */
public class MutationFactory {
	
	/**
	 * Return an operator based on mutation type. 
	 * Return null if type is not supported.
	 * 
	 * @param mutationType
	 * @return a IMutationOperator
	 */
	public static IMutationOperator getOperator(String mutationType){
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_ENUMERATION))
			return new EnumerationOperator();
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_MININCLUSIVE))
			return new MinInclusiveOperator();
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_MAXINCLUSIVE))
			return new MaxInclusiveOperator();
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_MAXLENGTH))
			return new MaxLengthOperator();
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_MINLENGTH))
			return new MinLengthOperator();
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_PATTERN))
			return new PatternOperator();
		if (mutationType.equals(IMutationOperator.XSD_MUTATION_WHITESPACE))
			return new WhiteSpaceOperator();
		return null;
	}
}
