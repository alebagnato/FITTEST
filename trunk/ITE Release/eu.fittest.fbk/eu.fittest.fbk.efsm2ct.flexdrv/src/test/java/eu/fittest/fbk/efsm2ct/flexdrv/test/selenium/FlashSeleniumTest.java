package eu.fittest.fbk.efsm2ct.flexdrv.test.selenium;

import static org.junit.Assert.*;

import org.junit.Test;

import com.thoughtworks.selenium.DefaultSelenium;
import com.thoughtworks.selenium.FlashSelenium;

public class FlashSeleniumTest {

	@Test
	public void test() {

		try {

			String configuration = "*firefox";
			
			DefaultSelenium ds = new DefaultSelenium("192.168.56.101", 4444,
					configuration, "http://localhost:8000/");
			ds.start();


			int timeout = 500;

			for (int i = 0; i < 1; i++) {

				ds.openWindow("flexstore.html", "w1");
				ds.selectWindow("w1");

				FlashSelenium fs = new FlashSelenium(ds, "AutomationLoader");

				try {
					Thread.sleep(250);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

				String[] app = { "flexstore.swf", "FlexDelegates.swf",
						"logger.swf" };

				boolean started = false;

				int attempts = 3;

				while (!started) {

					System.err.println("trying, attempts left:" + attempts);

					try {

						fs.call("loadApplication", app);
						started = true;

						if (timeout >= 2) {
							timeout /= 2;
						}

						System.err.println("started, next timeout:" + timeout);
					} catch (Exception ex) {
						System.err
								.println("error loading the flash app: " + ex);

						try {
							Thread.sleep(timeout);
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
						}

						timeout *= 2;
						System.err.println("next timeout:" + timeout);

						attempts--;

						if (attempts == 0) {

							break;

						}

					}

				}

				if (started) {

					System.out
							.println("you should see the application, do you?");

					try {
						Thread.sleep(5000);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}

				ds.close();
				ds.selectWindow("null");

			}

			try {
				Thread.sleep(5000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			ds.stop();
		} catch (Exception ex) {

			ex.printStackTrace();
			fail();
		}

	}

}
