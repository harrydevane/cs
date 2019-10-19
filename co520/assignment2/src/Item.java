
/**
 * Enumeration class Item
 * The items in the game.
 *
 * @author Olaf Chitil
 * @version 4/2/2019
 */
public enum Item
{
    FLOUR("flour"),

    SUGAR("sugar"),

    EGG("egg");

    private String description;

    private Item(String desc) {
        description = desc;
    }

    /**
     * Return the description of the item.
     */
    public String toString()
    {
        return description;
    }
}
