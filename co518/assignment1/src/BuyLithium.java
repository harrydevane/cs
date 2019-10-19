import java.util.Map;

public class BuyLithium {

    private final LithiumPricing lithiumPricing;

    public BuyLithium(LithiumPricing lithiumPricing) {
        this.lithiumPricing = lithiumPricing;
    }

    public void findBestPrice(double price) {
        int choices = 0;
        for (Map.Entry<Integer, Double> entry : lithiumPricing.getPrices().entrySet()) {
            int lithiumGrade = entry.getKey();
            double lithiumPrice = entry.getValue();
            if (lithiumPrice <= price) {
                System.out.println(lithiumGrade + " " + lithiumPrice);
                choices++;
            }
        }
        System.out.println("There are " + choices + " choices available to you.");
    }

    public void findHighQuality(int grade) {
        int choices = 0;
        for (Map.Entry<Integer, Double> entry : lithiumPricing.getPrices().entrySet()) {
            int lithiumGrade = entry.getKey();
            double lithiumPrice = entry.getValue();
            if (lithiumGrade >= grade) {
                System.out.println(lithiumGrade + " " + lithiumPrice);
                choices++;
            }
        }
        System.out.println("There are " + choices + " choices available to you.");
    }
}
