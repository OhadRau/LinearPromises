package lang.promises;

import java.util.concurrent.ExecutorService;

public class AsyncTask {
    private static long free_id = 0;
    private long id;
    private Runnable runnable;

    public AsyncTask(Runnable runnable) {
        this.id = free_id++;
        this.runnable = runnable;
    }

    public void run(ExecutorService service) {
        service.execute(this.runnable);
    }

    public long id() {
        return this.id;
    }
}
