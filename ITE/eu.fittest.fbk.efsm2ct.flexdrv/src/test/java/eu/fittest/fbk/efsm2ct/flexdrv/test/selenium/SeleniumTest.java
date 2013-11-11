package eu.fittest.fbk.efsm2ct.flexdrv.test.selenium;

import static org.junit.Assert.*;

import org.junit.Test;

import com.thoughtworks.selenium.DefaultSelenium;

public class SeleniumTest {

	@Test
	public void test() {

		DefaultSelenium ds = new DefaultSelenium("192.168.56.101", 4444,
				"*googlechrome", "http://localhost:8000/");
		ds.start();
		// ds.open("flexstore.html");
		// ds.close();

		for (int i = 0; i < 5; i++) {

			ds.openWindow("flexstore.html", "w1");
			ds.selectWindow("w1");

			
			try {
				Thread.sleep(3000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			String[] allWindowIds = ds.getAllWindowNames();

			int k = 1;
			
			for (String s : allWindowIds) {

				System.out.println("w#"+k+" = " + s);
				k++;

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
	}

}
