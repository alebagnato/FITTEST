package eu.fittest.fbk.efsm2ct.log2efsm.infer.formats;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class MutatorDescriptionSet {

	// List<MutatorDescriptor> mutators = new ArrayList<MutatorDescriptor>();
	Map<String, MutatorDescriptor> byName = new HashMap<String, MutatorDescriptor>();

	public void init(String mutatorTablesFn) throws FileNotFoundException, IOException {

		BufferedReader br = new BufferedReader(new FileReader(mutatorTablesFn));

		int alias = 1;

		while (br.ready()) {

			String line = br.readLine();

			String parts[] = line.split("/");

			String target = parts[0];
			String event = parts[1];
			int arity = Integer.parseInt(parts[2]);

			MutatorDescriptor md = new MutatorDescriptor(alias, target, event, arity);

			byName.put(md.getName(), md);

			alias++;

		}

		br.close();
	}

	public int size() {
		return byName.size();
	}

	public MutatorDescriptor getByName(String key) {
		return byName.get(key);
	}

	public Set<String> nameSet() {
		return byName.keySet();
	}

	public Collection<MutatorDescriptor> getValues() {
		return byName.values();
	}

}
