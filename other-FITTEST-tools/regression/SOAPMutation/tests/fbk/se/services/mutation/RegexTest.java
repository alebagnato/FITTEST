package fbk.se.services.mutation;

import org.junit.Test;

import fbk.se.services.mutation.utils.Generator;

public class RegexTest {

	@Test 
	public void generateString(){
		String regex = "P[0-9]{2}DT[1-2]?[0-9]H[1-5]?[0-9]M[1-5]?[0-9]S";
		String gen = Generator.generateRegexString(regex);
		System.out.println(gen);
		assert gen.matches(regex);
	}
}
