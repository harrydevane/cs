/**
 * The class Benefactor. Represents a Benefactor who has contributed
 * to a charity.
 *
 * @author Harry Devane
 * @version 27.11.2018
 */
public class Benefactor {

    private final String name;
    private int amount;
    private final boolean giftAid;

    /**
     * Constructor for Benefactor objects.
     *
     * @param name    The name of the benefactor.
     * @param amount  The amount the benefactor has contributed.
     * @param giftAid If the benefactor is a Gift Aider or not.
     */
    public Benefactor(String name, int amount, boolean giftAid) {
        this.name = name;
        this.amount = amount;
        this.giftAid = giftAid;
    }

    /**
     * Returns the name of the benefactor.
     *
     * @return The name of the benefactor.
     */
    public String getName() {
        return name;
    }

    /**
     * Returns the amount of money this benefactor has contributed.
     *
     * @return The amount of money this benefactor has contributed.
     */
    public int getAmount() {
        return amount;
    }

    /**
     * Returns if the benefactor is a Gift Aider or not.
     *
     * @return true if the benefactor is a Gift Aider.
     */
    public boolean isGiftAid() {
        return giftAid;
    }

    /**
     * Adds a contribution made by this benefactor.
     *
     * @param amount The contribition amount to add.
     */
    public void addContribution(int amount) {
        this.amount += amount;
    }

    /**
     * Returns the formatted details of this benefactor.
     *
     * @return The formatted details of this benefactor.
     */
    public String getDetails() {
        String details = "";
        details += name;
        details += " gave ";
        details += amount;
        if (giftAid) {
            details += "*";
        }
        return details;
    }
}
