package lang.promises;

@FunctionalInterface
public interface Function0<R> {
  R apply();
  public static <R> R call(Function0<R> fn) { return fn.apply(); }
}
