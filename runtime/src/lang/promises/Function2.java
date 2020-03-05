package lang.promises;

@FunctionalInterface
public interface Function2<R, A1, A2> {
  R apply(A1 a1, A2 a2);
}