package ocon.tests.diningphil;

import java.util.Random;
import java.util.concurrent.Semaphore;

public class Chopsticks {
	private Random r = new Random();
	private Semaphore s = new Semaphore(5);

	private boolean leftCS = false;
	private boolean rightCS = false;

	public void pickUp(int phil) {
		try {
			if (s.availablePermits() == 0) {
				System.out.println("Philosopher " +  phil +  " is THINKING!");
				Thread.sleep(r.nextInt(3000));
			} else {
				leftCS = s.tryAcquire();
				if (!leftCS) {
					System.out.println("Philosopher "  + phil +  " is THINKING!");
					Thread.sleep(r.nextInt(3000));
				} else {
					System.out.println("Philosopher " +  phil +  " GETS THE LEFT CHOPSTICK!");
					rightCS = s.tryAcquire();
					if (!rightCS) {
						System.out.println("Philosopher " +  phil +  " is WAITING!");
						Thread.sleep(r.nextInt(3000));
					} else {
						System.out.println("Philosopher "  +  phil +  " is EATING");
						Thread.sleep(r.nextInt(3000));
					}
				}
			}
		} catch (InterruptedException ie) {
			ie.printStackTrace();
		} finally {
			System.out.println("Philosopher " + phil +  " is THINKING!");
			s.release();
		}
	}
}
