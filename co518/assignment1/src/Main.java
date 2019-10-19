public class Main {

    public static void main(String[] args) {
        GenerateLithium generateLithium = new GenerateLithium();
        generateLithium.generateSample();
        generateLithium.printTray();

        System.out.println(" ");

        LithiumGrading lithiumGrading = new LithiumGrading(generateLithium);
        lithiumGrading.generateGrades(generateLithium.getTray());
        lithiumGrading.sortingLithium();

        System.out.println(" ");

        LithiumPricing lithiumPricing = new LithiumPricing(generateLithium);
        lithiumPricing.setPrice();
        lithiumPricing.printPrice();

        System.out.println(" ");

        BuyLithium buyLithium = new BuyLithium(lithiumPricing);
        buyLithium.findBestPrice(400);
        System.out.println(" ");
        buyLithium.findHighQuality(20);
    }
}
