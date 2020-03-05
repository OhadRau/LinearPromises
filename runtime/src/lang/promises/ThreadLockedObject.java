package lang.promises;

public class ThreadLockedObject {
  private AsyncTask myTask;

  public ThreadLockedObject() {
    this.myTask = AsyncTask.currentTask();
  }

  public boolean access() {
    return this.myTask.id() == AsyncTask.currentTask().id();
  }
}
