package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Instance;

public class MutatorsExtractor {

	Set<MutatorDescriptor> mds = new HashSet<MutatorDescriptor>();

	public void extractEvents(List<Event> events) {

		for (Event e : events) {

			Instance instance = e.getRecordEvent();

			TypedValue tvTargetId = instance.get("targetID");

			assert (tvTargetId != null);

			String target = (String) tvTargetId.getValue();

			TypedValue tvType = instance.get("type");

			String type = (String) tvType.getValue();

			TypedValue tvArgs = instance.get("args");

			Instance args = (Instance) tvArgs.getValue();

			TypedValue tvElem = args.get("elem");

			int arity = ((List<String>) tvElem.getValue()).size();

			// System.out.format("%s %s %d %s\n",target,type,arity,tvElem.getValue());

			mds.add(new MutatorDescriptor(target, type, arity));

		}

		// System.out.println();
		//
		// for (MutatorDescriptor md : mds) {
		//
		// System.out.println(md.getSignature());
		//
		// }

	}

	public Set<MutatorDescriptor> getMds() {
		return mds;
	}

}
