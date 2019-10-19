import java.util.HashMap;
import java.util.Map;

public class LithiumPricing {

    private final GenerateLithium generateLithium;
    private final Map<Integer, Double> prices = new HashMap<>();

    public LithiumPricing(GenerateLithium generateLithium) {
        this.generateLithium = generateLithium;
    }

    public void setPrice() {
        int[][] tray = generateLithium.getTray();
        for (int i = 0; i < tray.length; i++) {
            for (int j = 0; j < tray[i].length; j++) {
                int grade = tray[i][j];
                if (grade <= 9) {
                    prices.put(grade, 300.0);
                } else if (grade <= 19) {
                    prices.put(grade, 600.0);
                } else if (grade <= 29) {
                    prices.put(grade, 900.0);
                } else {
                    prices.put(grade, 1250.0);
                }
            }
        }
    }

    public void printPrice() {
        for (Map.Entry<Integer, Double> entry : prices.entrySet()) {
            int grade = entry.getKey();
            double price = entry.getValue();
            System.out.println(grade + " " + price);
        }
    }

    public Map<Integer, Double> getPrices() {
        return prices;
    }
}
