package lang.promises;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Promise<T> extends ThreadLockedObject {
    private T value;
    private boolean done;
    private Lock lock;

    public Promise() {
        super();
        done = false;
        lock = new ReentrantLock();
    }

    public Unit fulfill(T value) {
        synchronized (lock) {
            lock.lock();
            this.value = value;
            done = true;
            lock.notifyAll();
            lock.unlock();
        }
        return Unit.the;
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
