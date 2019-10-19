import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JTextArea;
import java.awt.BorderLayout;

public class MyFigure extends JFrame {

    private final JButton invert;
    private final JTextArea canvas;
    private final GUIFigure guif;

    public MyFigure() {
        this.invert = new JButton();
        this.invert.setText("invert");

        this.canvas = new JTextArea();
        this.canvas.setRows(Figure.BOUND);
        this.canvas.setColumns(Figure.BOUND);

        this.guif = new GUIFigure();
        this.guif.setPred((int1, int2) -> int1 < int2);
        this.guif.setTextArea(this.canvas);

        this.invert.addActionListener(event -> this.guif.invert());

        setLayout(new BorderLayout());

        add(this.invert, BorderLayout.NORTH);
        add(this.canvas, BorderLayout.SOUTH);

        setSize(400, 200);
        setVisible(true);
    }
}
