package lang.promises;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Runtime {
    private static final int MAX_THREADS = 10;

    private ExecutorService pool;
    private int num_threads;

    public Runtime() {
        this(MAX_THREADS);
    }

    public Runtime(int num_threads) {
        this.num_threads = num_threads;
        pool = Executors.newFixedThreadPool(num_threads);
    }

    public void async(Runnable r) {
        pool.execute(r);
    }
    public void async(AsyncTask t) { t.run(pool); }

    public static void main(String[] args) {
        Runtime rt = new Runtime();
        Promise<Integer> pInt = new Promise<Integer>();
        rt.async(new AsyncTask(() -> {
            System.out.println(pInt.get() * 2);
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
