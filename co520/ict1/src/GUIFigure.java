import javax.swing.JTextArea;

public class GUIFigure extends AbstractFigure {

    private JTextArea area;

    public void setTextArea(JTextArea area) {
        this.area = area;
    }

    @Override
    public void printFigure() {
        area.setText(toString());
    }

    @Override
    public void invert() {
        super.invert();
        printFigure();
    }
}
