package eu.fittest.fbk.efsm2ct.log2efsm;

import org.junit.Test;

import eu.fittest.fbk.efsm2ct.BaseTest;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.Main;

public class Test01LogConvMain extends BaseTest {

	// @Test
	public void test1_1() {

		String dir = "target/output/fsm_old";

		createDirectory(dir);

		Main.main(new String[] { "src/test/input/vold", "flexstore", dir });

	}

	// @Test
	public void test1_2() {

		String dir = "target/output/fsm_v1";

		createDirectory(dir);

		Main.main(new String[] { "src/test/input/v1", "flexstore", dir });

	}

	// @Test
	public void test1_3() {

		String dir = "target/output/fsm_v3";

		createDirectory(dir);

		Main.main(new String[] { "src/test/input/v3", "flexstore", dir });

	}

	// @Test
	public void test1_5898() {

		String dir = "target/output/fsm_5898";

		createDirectory(dir);

		Main.main(new String[] { "src/test/input/v_5898", "flexstore", dir });

	}

	@Test
	public void test1_5898_1() {

		String dir = "target/output/fsm_5898_1";

		createDirectory(dir);

		Main.main(new String[] { "src/test/input/v_5898_1", "flexstore", dir });

	}

	// @Test
	public void test2() {

		Main.main(new String[] {});

	}

	// @Test
	public void test3() {

		Main.main(new String[] { "-r" });

	}

	@Test
	public void test_v4() {

		String dir = "target/output/v4";

		createDirectory(dir);

		Main.main(new String[] { "src/test/input/v4", "flexstore", dir });

	}

}
