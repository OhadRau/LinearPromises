package lang.promises;

@FunctionalInterface
public interface Function5<R, A1, A2, A3, A4, A5> {
  R apply(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5);
}