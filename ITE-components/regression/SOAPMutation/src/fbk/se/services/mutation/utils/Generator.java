package fbk.se.services.mutation.utils;

import java.util.Random;

import nl.flotsam.xeger.Xeger;

/**
 * String generator
 * 
 * @author cdnguyen
 *
 */
public class Generator {
	private static String TOKENS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
	private static Random ranGenerator = new Random();
	
	/**
	 * Generate randomly a string of length: len
	 * @param len
	 * @return
	 */
	public static String generateString(int len){
		String ret = "";
		for (int i = 0; i < len; i++) {
			ret += TOKENS.charAt(ranGenerator.nextInt(TOKENS.length()));
		}
		return ret;
	}

	/**
	 * Generate a string that matches a regular expression
	 * @param regex
	 * @return
	 */
	public static String generateRegexString(String regex){
		Xeger generator = new Xeger(regex);
		String result = generator.generate();
		return result;
	}
}
