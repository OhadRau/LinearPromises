package lang.promises;

@FunctionalInterface
public interface Function3<R, A1, A2, A3> {
  R apply(A1 a1, A2 a2, A3 a3);
}