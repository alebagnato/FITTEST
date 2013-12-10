package ocon.tests.diningphil;

import java.util.Random;

public class Philosopher implements Runnable {
	private Random r = new Random();

	private Chopsticks rightStick;
	private Chopsticks leftStick;

	private int phil;

	public Philosopher(Chopsticks r, Chopsticks l, int phil) {
		this.rightStick = r;
		this.leftStick = l;
		this.phil = phil;
		Thread t1 = new Thread(this);
		t1.start();
	}

	public void run() {
		for (int i = 1; i <= 1; i++ ) {
			try {
				Thread.sleep(r.nextInt(3000));
				rightStick.pickUp(phil);
				leftStick.pickUp(phil);
			} catch (InterruptedException ie) {
				ie.printStackTrace();
			}
		}
	}
}

