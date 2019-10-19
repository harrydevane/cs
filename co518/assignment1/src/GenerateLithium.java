import java.util.Random;

public class GenerateLithium {

    private final int[][] tray = new int[5][3];
    private final Random random = new Random();

    public void generateSample() {
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 3; j++) {
                tray[i][j] = random.nextInt(50) + 1;
            }
        }
    }

    public void printTray() {
        for (int i = 0; i < tray.length; i++) {
            for (int j = 0; j < tray[i].length; j++) {
                if (j > 0) {
                    if (tray[i][j - 1] < 10) {
                        System.out.print(" ");
                    }
                    System.out.print(" || ");
                }
                System.out.print(tray[i][j]);
            }
            System.out.println();
        }
    }

    public int[][] getTray() {
        return tray;
    }
}
