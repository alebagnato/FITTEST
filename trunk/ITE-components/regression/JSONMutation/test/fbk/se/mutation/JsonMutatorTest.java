package fbk.se.mutation;

import java.io.File;
import java.util.List;

import org.junit.Test;

import com.fasterxml.jackson.databind.JsonNode;
import fbk.se.mutation.utils.FileUtils;

public class JsonMutatorTest {
	
	private String projectPath = System.getProperty("user.dir");

	@Test
	public void testMutateJsonNode() {
		String inputConstraint = projectPath + File.separator + "xsd" + File.separator + "SCloud.xml"; 
		String inputJsonFile =   projectPath + File.separator + "testdata" + File.separator + "RES-15-46.json";
		String outpurDir =  projectPath + File.separator + "testdata";
		
		JsonMutator mutator = new JsonMutator(inputConstraint);
		JsonNode originalDoc = FileUtils.load(new File(inputJsonFile));
		List<JsonNode> mutants = mutator.mutate(originalDoc);

		// save to dir
		if (mutants != null){
			for (int i = 0; i < mutants.size(); i++){
				JsonNode mutant = mutants.get(i);
				String outFileName = outpurDir + File.separatorChar + "RES-Mutant-" + String.valueOf(i) + ".json";
				FileUtils.save(mutant, outFileName);
			}
		}
	}

}
