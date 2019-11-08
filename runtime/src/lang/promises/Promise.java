package lang.promises;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Promise<T> {
    private T value;
    private boolean done;
    private Lock lock;
    private AsyncTask owner;

    public Promise() {
        done = false;
        lock = new ReentrantLock();
    }

    public Promise(AsyncTask task) {
        owner = task;
    }

    public void fulfill(T value) {
        synchronized (lock) {
            lock.lock();
            this.value = value;
            done = true;
            lock.notifyAll();
            lock.unlock();
        }
    }

    public T get() {
        T result;
        synchronized (lock) {
            if (!done) {
                try {
                    lock.wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }

        lock.lock();
        synchronized (value) {
            result = value;
        }
        lock.unlock();
        return result;
    }
}
