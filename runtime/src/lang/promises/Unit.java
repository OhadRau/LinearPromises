package lang.promises;

public class Unit {
    private Unit() {}
    public static final Unit the = new Unit();
    public static Unit ignore(Object x) { return the; }
}
