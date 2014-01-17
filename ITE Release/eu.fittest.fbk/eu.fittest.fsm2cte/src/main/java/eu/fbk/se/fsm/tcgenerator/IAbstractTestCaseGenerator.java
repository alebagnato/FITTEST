package eu.fbk.se.fsm.tcgenerator;

import eu.fbk.se.fsm.cte.CteObject;

public interface IAbstractTestCaseGenerator {
	public boolean generateTestCases(CteObject cteTree);
}
