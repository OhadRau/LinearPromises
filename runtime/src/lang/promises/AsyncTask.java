package lang.promises;

import java.util.concurrent.ExecutorService;

public class AsyncTask {
    private static long free_id = 0;
    private static ThreadLocal<AsyncTask> threadTask = new ThreadLocal<>();
    private long id;
    private Function0<Unit> runnable;

    public static AsyncTask currentTask() {
        return threadTask.get();
    }

    public AsyncTask(Function0<Unit> runnable) {
        this.id = free_id++;
        this.runnable = runnable;
    }

    public void run(ExecutorService service) {
        service.execute(() -> {
            threadTask.set(this);
            this.runnable.apply();
        });
    }

    public long id() {
        return this.id;
    }
}
