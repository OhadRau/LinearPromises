package lang.promises;

import java.util.concurrent.CompletableFuture;

public class Promise<T> {
    private CompletableFuture<T> promise;

    public Promise() {
        promise = new CompletableFuture<>();
    }

    public Unit fulfill(T value) {
        // Runtime allows multiple completions because
        // we use promises synchronously as a ref cell:
        if (promise.isDone()) {
            promise.obtrudeValue(value); // Replace the value anyways!
        } else {
            promise.complete(value);
        }
        return Unit.the;
    }

    public T get() {
        return promise.join();
    }
}
