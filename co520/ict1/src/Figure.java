public class Figure extends AbstractFigure {

    public static int BOUND = 7;

    @Override
    public void printFigure() {
        System.out.println(toString());
    }

    public static BiIntPredicate negate(BiIntPredicate x) {
        return (int1, int2) -> !x.test(int1, int2);
    }
}
