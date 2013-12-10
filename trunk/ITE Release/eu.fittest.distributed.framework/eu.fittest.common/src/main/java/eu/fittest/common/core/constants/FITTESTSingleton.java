package eu.fittest.common.core.constants;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import eu.fittest.common.core.xml.ObjectFactory;
import eu.fittest.common.util.ITELogger;

public class FITTESTSingleton {
	
	private static final int MONITOR_REPORT_INTERVAL = 60; // in seconds
	private static final int THREAD_PER_CPU = 10; // limit number of threads per CPU
	
	private static int maxThreads = 0;
	
	private static ThreadPoolExecutor _threadPool = null;
	private static ObjectFactory _objectFactory = new ObjectFactory();
	private static MyMonitorThread monitor = null;

	public static synchronized ThreadPoolExecutor getThreadPool() {
		if (_threadPool != null && !_threadPool.isShutdown())
			return _threadPool;
		else {
			initPool();
			return _threadPool;
		}
	}
	
	public static void setThreadPoolSize(int size){
		if (size < THREAD_PER_CPU){
			maxThreads = THREAD_PER_CPU;
		} else {
			maxThreads = size;
		}
	}
	
	private static int getThreadPoolSize(){
		if (maxThreads == 0){
			int cpus = Runtime.getRuntime().availableProcessors();
			maxThreads = cpus * THREAD_PER_CPU;
			maxThreads = (maxThreads < FITTESTConstants.MIN_THREAD_POOL_SIZE ? FITTESTConstants.MIN_THREAD_POOL_SIZE : maxThreads);
			
			if (maxThreads > FITTESTConstants.MAX_THREAD_POOL_SIZE)
				maxThreads = FITTESTConstants.MAX_THREAD_POOL_SIZE;

		}
		return maxThreads;
	}

	public static ObjectFactory getObjectFactory() {
		return _objectFactory;
	}

	/**
	 * Shutdown threadpool
	 */
	public static void shutdown() {
		
		ITELogger.log(Level.INFO, "Shutting down the threadpool");
		_threadPool.shutdown();

		try {
			if (!_threadPool.awaitTermination(60, TimeUnit.SECONDS)) {
				// pool didn't terminate after the first try
				_threadPool.shutdownNow();
			}

			if (!_threadPool.awaitTermination(60, TimeUnit.SECONDS)) {
				ITELogger.log(Level.SEVERE, "Cannot shutdown the threadpool");
			}
		} catch (InterruptedException ex) {
			_threadPool.shutdownNow();
			Thread.currentThread().interrupt();
		}
		
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        monitor.shutdown();
	}

	private static void initPool() {
		// RejectedExecutionHandler implementation
		RejectedExecutionHandlerImpl rejectionHandler = new RejectedExecutionHandlerImpl();
		// Get the ThreadFactory implementation to use

		ThreadFactory threadFactory = Executors.defaultThreadFactory();
		// creating the ThreadPoolExecutor
		
		int size = getThreadPoolSize();
		
		_threadPool = new ThreadPoolExecutor(maxThreads, maxThreads, 10,
				TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(size, true),
				threadFactory, rejectionHandler);
		// start the monitoring thread
		monitor = new MyMonitorThread(_threadPool, MONITOR_REPORT_INTERVAL);
		Thread monitorThread = new Thread(monitor);
		monitorThread.start();

	}

	private static class RejectedExecutionHandlerImpl implements
			RejectedExecutionHandler {

		@Override
		public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
			ITELogger.log(Level.SEVERE, r.toString() + " is rejected");
		}

	}

	private static class MyMonitorThread implements Runnable {
		private ThreadPoolExecutor executor;
		private int seconds;
		private boolean run = true;

		public MyMonitorThread(ThreadPoolExecutor executor, int delay) {
			this.executor = executor;
			this.seconds = delay;
		}

		public void shutdown() {
			this.run = false;
		}

		@Override
		public void run() {
			while (run) {
				String s = String
						.format("[monitor] [%d/%d] Active: %d, Completed: %d, Task: %d, isShutdown: %s, isTerminated: %s",
								executor.getPoolSize(),
								executor.getCorePoolSize(),
								executor.getActiveCount(),
								executor.getCompletedTaskCount(),
								executor.getTaskCount(),
								executor.isShutdown(),
								executor.isTerminated());
				ITELogger.log(Level.INFO, s);
				
				try {
					Thread.sleep(seconds * 1000);
				} catch (Exception e) {
					e.printStackTrace();
				}

			}
		}

	}

}
