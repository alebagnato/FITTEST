package ocon.tests.queue;

import java.util.*;

public class PrintQueue {
   private LinkedList<String> queue = new LinkedList<String>();
   private final Object lock = new Object();

   public void enqueue(String str) {
      synchronized (lock) {
         queue.addLast(str);
         lock.notifyAll();
      }
   }

   public void work() {
      String current;
      synchronized(lock) {
         if (queue.isEmpty()) {
            try {
               lock.wait();
            } catch (InterruptedException e) {
               assert (false);
            }
         }
         current = queue.removeFirst();
      }
      System.out.println(current);
   }

   public static void main(String[] args) {
      final PrintQueue pq = new PrintQueue();

      Thread producer1 = new Thread() {
         public void run() {
            pq.enqueue("anemone");
            pq.enqueue("tulip");
            pq.enqueue("cyclamen");
         }
      };

      Thread producer2 = new Thread() {
         public void run() {
            pq.enqueue("iris");
            pq.enqueue("narcissus");
            pq.enqueue("daffodil");
         }
      };

      Thread consumer1 = new Thread() {
         public void run() {
            pq.work();
            pq.work();
            pq.work();
            pq.work();
         }
      };

      Thread consumer2 = new Thread() {
         public void run() {
            pq.work();
            pq.work();
         }
      };

      producer1.start();
      consumer1.start();
      consumer2.start();
      producer2.start();
   }
}
