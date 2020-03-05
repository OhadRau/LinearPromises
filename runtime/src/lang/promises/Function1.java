package lang.promises;

@FunctionalInterface
public interface Function1<R, A1> {
  R apply(A1 a1);
}