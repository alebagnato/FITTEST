package ocon.tests.diningphil;

public class PhilosopherTest {

	public static void main(String[] args) {
		Chopsticks cs = new Chopsticks();
		Philosopher p1 = new Philosopher(cs, cs, 1);
		Philosopher p2 = new Philosopher(cs, cs, 2);
		Philosopher p3 = new Philosopher(cs, cs, 3);
		Philosopher p4 = new Philosopher(cs, cs, 4);
		Philosopher p5 = new Philosopher(cs, cs, 5);

		Thread t1 = new Thread(p1);
		t1.start();
		Thread t2 = new Thread(p2);
		t2.start();
		Thread t3 = new Thread(p3);
		t3.start();
		Thread t4 = new Thread(p4);
		t4.start();
		Thread t5 = new Thread(p5);
		t5.start();
	}
}
