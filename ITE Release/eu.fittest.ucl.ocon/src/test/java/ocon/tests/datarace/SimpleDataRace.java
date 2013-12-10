package ocon.tests.datarace;

public class SimpleDataRace {
	
	private static int i = 0;

	public static class Incrementor extends Thread {
		public void run() {
			synchronized (SimpleDataRace.class) {
				i++;
			}
		}
	}
	public static int GetI(){return i;}
}
