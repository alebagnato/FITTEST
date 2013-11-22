package fbk.se.mutation.utils;

import java.io.File;
import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;

public class FileUtils {
	
	/**
	 * Load json string and return the root tree
	 * @param jsonFile
	 * @return
	 */
	public static JsonNode load(String jsonContent){
		ObjectMapper mapper = new ObjectMapper();
		JsonNode root;
		try {
			root = mapper.readTree(jsonContent);
			return root;
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Load json file and return the root tree
	 * @param jsonFile
	 * @return
	 */
	public static JsonNode load(File jsonFile){
		ObjectMapper mapper = new ObjectMapper();
		JsonNode root;
		try {
			root = mapper.readTree(jsonFile);
			return root;
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	
	/**
	 * Save a jason node to file
	 * @param root
	 * @param fileName
	 */
	public static void save(JsonNode root, String fileName){
		ObjectMapper mapper = new ObjectMapper();
		ObjectWriter w = mapper.writer().with(
				SerializationFeature.INDENT_OUTPUT);
		try {
			w.writeValue(new File(fileName), root);
		} catch (JsonGenerationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JsonMappingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	

	
	/**
	 * Check if file or directory exist
	 * @param fileordir
	 * @return
	 */
	public static int checkFileDir(String fileordir){
		File f = new File(fileordir);
		if (!f.exists()) 
			return -1;
		else if (f.isDirectory())
			return 0;
		else return 1;
		
	}
}
