package fbk.se.mutation.utils;

import static org.junit.Assert.*;

import java.io.File;
import java.util.List;

import org.junit.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class JsonUtilsTest {

	@Test
	public void testQuery() {
		String fullPath = System.getProperty("user.dir") + File.separator
				+ "etc/RES-15-46.json";

		// Target
		JsonNode root = FileUtils.load(new File(fullPath));
		assertNotNull(root);
		
		List<JsonNode> queryResults;
		try {
			queryResults = JsonUtils.query("/targets/name", root);
			assertEquals(2, queryResults.size());

			queryResults = JsonUtils.query("/targets/system/uri", root);
			assertEquals(2, queryResults.size());
			
			queryResults = JsonUtils.query("/targets/system", root);
			JsonNode first = queryResults.get(0);
			((ObjectNode)first).put("uri", "new URI");

			System.out.println(root.toString());
			
		} catch (PathException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
