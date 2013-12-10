package eu.fittest.ucl.watt;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class DatabaseTest {
	
	@Test
	public void testDBSetup() {
		Database db = new Database("root", "mypass");
		assertTrue(db.runSQLSetupScript());
	}
}
