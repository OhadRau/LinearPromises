package lang.promises;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class PromiseRuntime {
    private static final int MAX_THREADS = 10;

    private ExecutorService pool;
    private int num_threads;

    public PromiseRuntime() {
        this(MAX_THREADS);
    }

    public PromiseRuntime(int num_threads) {
        this.num_threads = num_threads;
        pool = Executors.newFixedThreadPool(num_threads);
    }

    public Unit async(Runnable r) {
        pool.execute(r);
        return Unit.the;
    }

    public Unit async(AsyncTask t) {
        t.run(pool);
        return Unit.the;
    }

    public Unit shutdown() {
        pool.shutdown();
        try {
            // Long.MAX_VALUE is treated as forever in awaitTermination
            pool.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            return Unit.the;
        }
        return Unit.the;
    }

    public static void main(String[] args) {
        PromiseRuntime rt = new PromiseRuntime();
        Promise<Integer> pInt = new Promise<Integer>();
        rt.async(new AsyncTask(() -> {
            System.out.println("Thread id: " + AsyncTask.currentTask().id());
            System.out.println(pInt.get() * 2);
            return Unit.the;
        }));
        rt.async(() -> {
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            pInt.fulfill(5);
        });
    }
}
