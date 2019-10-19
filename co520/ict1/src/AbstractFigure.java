public abstract class AbstractFigure {

    private BiIntPredicate pred = (int1, int2) -> false;

    public void setPred(BiIntPredicate pred) {
        this.pred = pred;
    }

    public BiIntPredicate getPred() {
        return pred;
    }

    public void invert() {
        pred = Figure.negate(pred);
    }

    @Override
    public String toString() {
        String result = "";
        for (int i = 0; i < Figure.BOUND; i++) {
            for (int j = 0; j < Figure.BOUND; j++) {
                if (pred.test(i, j)) {
                    result += "*";
                }
            }
            result += "\n";
        }
        return result;
    }

    public abstract void printFigure();
}
