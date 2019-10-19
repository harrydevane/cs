import java.util.ArrayList;
import java.util.List;

public class LithiumGrading {

    private final GenerateLithium generateLithium;
    private final List<Integer> lowGrade = new ArrayList<>();
    private final List<Integer> highGrade = new ArrayList<>();

    public LithiumGrading(GenerateLithium generateLithium) {
        this.generateLithium = generateLithium;
    }

    public void generateGrades(int[][] tray) {
        for (int i = 0; i < tray.length; i++) {
            for (int j = 0; j < tray[i].length; j++) {
                int grade = tray[i][j];
                if (grade <= 25) {
                    lowGrade.add(grade);
                } else {
                    highGrade.add(grade);
                }
            }
        }
    }

    public void sortingLithium() {
        bubbleSortList(lowGrade);
        bubbleSortList(highGrade);

        System.out.println("High grade");
        for (int grade : highGrade) {
            System.out.println(grade);
        }

        System.out.println("Low grade");
        for (int grade : lowGrade) {
            System.out.println(grade);
        }
    }

    private void bubbleSortList(List<Integer> list) {
        for (int i = list.size() - 1; i > 0; i--) {
            for (int j = 0; j < i; j++) {
                if (list.get(j) > list.get(j + 1)) {
                    int temp = list.get(j);
                    list.set(j, list.get(j + 1));
                    list.set(j + 1, temp);
                }
            }
        }
    }
}
