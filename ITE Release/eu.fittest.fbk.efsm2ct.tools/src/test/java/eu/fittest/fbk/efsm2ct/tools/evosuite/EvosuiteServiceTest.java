package eu.fittest.fbk.efsm2ct.tools.evosuite;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.net.Socket;

import org.junit.Test;

public class EvosuiteServiceTest {

	@Test
	public void testRun() throws IOException {

		System.out.println("user.dir=" + System.getProperty("user.dir"));

		EvosuiteService service = new EvosuiteService("tobetested.UnderTest");
		service.setEvosuiteDirPath("lib");
		service.setWorkingDirectory("target");
		service.addClasspath("../src/test/classes");
		service.addProperty(EvosuiteService.print_to_system, "true");

		service.init();

		int status;

		try {
			status = service.run();
			assertEquals(0, status);
		} catch (ProcessSpawnException e) {
			e.printStackTrace();
			fail();
		}

	}

	@Test
	public void testRunWithLogger() throws IOException {

		System.out.println("user.dir=" + System.getProperty("user.dir"));

		EvosuiteService service = new EvosuiteService("tobetested.UnderTest");
		service.setEvosuiteDirPath("lib");
		service.setWorkingDirectory("target");
		service.addClasspath("../src/test/classes");
		service.addProperty(EvosuiteService.print_to_system, "true");
		service.addLogConsumer(new LogConsumer() {

			@Override
			public void consume(char ch) {
				System.out.print(ch);

			}
		});

		service.init();

		int status;

		try {
			status = service.run();
			assertEquals(0, status);
		} catch (ProcessSpawnException e) {
			e.printStackTrace();
			fail();
		}

	}

	@Test
	public void testStop() throws IOException {

		System.out.println("user.dir=" + System.getProperty("user.dir"));

		EvosuiteService service = new EvosuiteService("tobetested.LongUnderTest");
		service.setEvosuiteDirPath("lib");
		service.setWorkingDirectory("target");
		service.addClasspath("../src/test/classes");
		service.addProperty(EvosuiteService.print_to_system, "true");
		
		service.addLogConsumer(new LogConsumer() {

			@Override
			public void consume(char ch) {
				System.out.print(ch);

			}
		});

		service.setKillChecker(new LogConsumer() {

			private int xes = 0;
			private boolean running = true;

			@Override
			public void consume(char ch) {

				boolean res = false;

				if (ch == 'X') {

					xes++;

					res = xes > 1000;
					
					if (res == true && running) {
					System.out.println("terminating evosuite");
					terminate();
					running = false;
					
					}

				}

				

			}
			
			public void terminate()  {

				try {
					Socket s = new Socket("localhost", 7000); // TODO remove host ip address
					s.close();
				} catch (Exception e) {
					e.printStackTrace();
					// throw e;
				}

			}
			
		});

		service.init();

		int status;

		try {

			status = service.run();
			assertEquals(0, status);

		} catch (ProcessSpawnException e) {
			e.printStackTrace();
			fail();
		}

	}

}
