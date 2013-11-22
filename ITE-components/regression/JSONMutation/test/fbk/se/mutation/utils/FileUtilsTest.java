package fbk.se.mutation.utils;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Iterator;
import java.util.List;

import org.junit.Test;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class FileUtilsTest {

	@Test
	public void testLoadFile() {
		String fullPath = System.getProperty("user.dir") + File.separator
				+ "etc/ex1.json";
		JsonNode root = FileUtils.load(new File(fullPath));
		
		assertNotNull(root);
		System.out.println("Hello " + root.get("name"));
		JsonNode type = root.path("other");
		System.out.println(type.get("type").asText());
		
	}

	@Test
	public void testSave() {
		String fullPath = System.getProperty("user.dir") + File.separator
				+ "etc/RES-15-46.json";

		// Target
		JsonNode root = FileUtils.load(new File(fullPath));
		assertNotNull(root);

		JsonNode targets = root.get("targets");
		if (targets.isArray()) {
			Iterator<JsonNode> iter = targets.elements();
			while (iter.hasNext()) {
				JsonNode node = iter.next();
				System.out.println(node.get("name").asText());
			}
		}

		List<JsonNode> list = root.findValues("system");
		for (JsonNode n : list) {
			System.out.println(n.get("uri").asText());
			((ObjectNode) n).put("uri", "new uri");
		}
		
		String fileName = System.getProperty("user.dir")  + File.separator + "user-modified.json";
		FileUtils.save(root, fileName);
	}

}
