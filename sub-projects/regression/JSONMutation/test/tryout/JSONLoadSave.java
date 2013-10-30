package tryout;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.NullNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class JSONLoadSave {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		test2();
	}

	/**
	 * Hello world test
	 */
	public static void test1() {
		ObjectMapper mapper = new ObjectMapper(); // create once, reuse
		// can be read as generic JsonNode, if it can be Object or Array; or,
		// if known to be Object, as ObjectNode, if array, ArrayNode etc:
		try {
			String fullPath = System.getProperty("user.dir") + File.separator
					+ "etc/ex1.json";
			JsonNode root = mapper.readTree(new File(fullPath));

			System.out.println("Hello " + root.get("name"));
			JsonNode type = root.path("other");
			System.out.println(type.get("type").asText());

		} catch (JsonProcessingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * More complicated test
	 */
	public static void test2() {
		ObjectMapper mapper = new ObjectMapper(); // create once, reuse
		try {
			String fullPath = System.getProperty("user.dir") + File.separator
					+ "etc/RES-15-46.json";

			// Target
			JsonNode root = mapper.readTree(new File(fullPath));
			System.out.println("URI: " + root.get("uri"));

//			JsonNode targets = root.get("targets");
//			if (targets.isArray()) {
//				Iterator<JsonNode> iter = targets.elements();
//				while (iter.hasNext()) {
//					JsonNode node = iter.next();
//					System.out.println(node.get("name").asText());
//				}
//			}
			
			// try null
//			JsonNode cannotBeFound = root.get("aaa");
//			System.out.println(cannotBeFound);
//			cannotBeFound = root.path("aaa");
//			System.out.println(cannotBeFound.isMissingNode());
//			
//			cannotBeFound = root.with("aaa");
//			System.out.println(cannotBeFound);
			
			
			// Try to query
			JsonNode result = root.path("targets");
			System.out.println(result.isArray());
			

//			List<JsonNode> list = root.findValues("system");
//			for (JsonNode n : list) {
//				System.out.println(n.get("uri").asText());
//				((ObjectNode) n).put("uri", "new uri");
//			}
//			ObjectWriter w = mapper.writer().with(
//					SerializationFeature.INDENT_OUTPUT);
//			w.writeValue(new File(System.getProperty("user.dir")
//					+ File.separator + "user-modified.json"), root);

		} catch (JsonProcessingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}