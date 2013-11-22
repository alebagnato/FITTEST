package fbk.se.mutation.xpath;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

public class VerySimpleXpathParserTest {

	@Test
	public void testPathElement() {
		VerySimpleXpathParser parser = new VerySimpleXpathParser("/a/b/d");
		List<String> paths = parser.getFullPath();
		for (String e : paths){
			System.out.println(e);
		}
		assertEquals(2, paths.size());
		assertEquals("b", paths.get(1));
	}
	
	@Test
	public void testFieldName(){
		VerySimpleXpathParser parser = new VerySimpleXpathParser("/a/b/f123");
//		assertEquals("f123", parser.getTargetFieldName());
	}

}
