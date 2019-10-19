import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

/**
 * The class ChasingBombs. Represents a chasing bombs game.
 *
 * @author Harry Devane
 * @version 2019.03.14
 */
public class ChasingBombs extends JFrame {

    private int pointsToWin = 5;
    private int points;
    private boolean finished;
    private final List<JPanel> squares = new ArrayList<>();
    private JPanel bomb;
    private JLabel resultLabel;

    /**
     * Constructor for ChasingBombs objects.
     */
    public ChasingBombs() {
        super("Chasing Bombs");

        setSize(600, 300);
        setResizable(false);

        setLayout(new GridLayout(1, 3));

        addPanelA();
        addPanelB();
        addPanelC();

        setVisible(true);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    /**
     * Adds panel A to the frame.
     */
    private void addPanelA() {
        JPanel panel = new JPanel();

        panel.setLayout(new GridLayout(2, 5, 2, 2));

        for (int i = 0; i < 10; i++) {
            JPanel square = new JPanel();

            square.setBackground(Color.BLUE);

            square.addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(MouseEvent event) {
                    if (square.getBackground() == Color.BLUE && !finished) {
                        if (square == bomb) {
                            finished = true;
                            resultLabel.setText("You lose! You got " + points + " points");
                            resultLabel.setVisible(true);
                        } else {
                            points++;
                            square.setBackground(Color.GRAY);
                            if (points == pointsToWin) {
                                finished = true;
                                resultLabel.setText("You win! You got " + points + " points");
                                resultLabel.setVisible(true);
                            }
                        }
                    }
                }
            });

            panel.add(square);

            squares.add(square);
        }

        bomb = squares.get((int) (Math.random() * squares.size()));

        add(panel);
    }

    /**
     * Adds panel B to the frame.
     */
    private void addPanelB() {
        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        JButton playButton = new JButton("Play A Game");
        playButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        playButton.addActionListener(event -> {
            points = 0;
            finished = false;
            bomb = squares.get((int) (Math.random() * squares.size()));
            for (JPanel square : squares) {
                square.setBackground(Color.BLUE);
            }
            resultLabel.setVisible(false);
        });
        panel.add(playButton);

        JButton exitButton = new JButton("Exit");
        exitButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        exitButton.addActionListener(event -> System.exit(0));
        panel.add(exitButton);

        resultLabel = new JLabel("");
        resultLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(resultLabel);

        panel.setBackground(Color.WHITE);

        add(panel);
    }

    /**
     * Adds panel C to the frame.
     */
    private void addPanelC() {
        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        JButton easyButton = new JButton("Easy");
        easyButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        easyButton.addActionListener(event -> pointsToWin = 5);
        panel.add(easyButton);

        JButton intermediateButton = new JButton("Intermediate");
        intermediateButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        intermediateButton.addActionListener(event -> pointsToWin = 7);
        panel.add(intermediateButton);

        JButton difficultButton = new JButton("Difficult");
        difficultButton.setAlignmentX(Component.CENTER_ALIGNMENT);
        difficultButton.addActionListener(event -> pointsToWin = 9);
        panel.add(difficultButton);

        panel.setBackground(Color.RED);

        add(panel);
    }
}
