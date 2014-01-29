package ocon.tests.datarace;

import ocon.tests.datarace.SimpleDataRace.Incrementor;

public class SimpleDataRaceTest {
	private final static int NUM = 10;
	public static void main(String[] args)
	{
		Incrementor[] incrementors = new Incrementor[NUM];
		int i = 0;
		for (; i < NUM; i++) {
			incrementors[i] = new Incrementor();
		}
		for (Incrementor inc : incrementors) {
			inc.start();
		}
		for (Incrementor inc : incrementors) {
			try {
				inc.join();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		System.out.println("final value: " + SimpleDataRace.GetI());
		if (SimpleDataRace.GetI() != NUM) {
			System.err.println("   Bug - expected " + NUM);
			throw new RuntimeException("CONC BUG");
		}
	}
}
