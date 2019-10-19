
/**
 * The class Medic. Represents a Medic.
 *
 * @author Harry Devane
 * @version 2018.12.08
 */
public class Medic extends Person
{

    /**
     * Constructor for Medic objects.
     * @param name The medic's name.
     * @param title The medic's title.
     */
    public Medic(String name, String title)
    {
        super(name, title);
    }

    /**
     * Return the title and name of this medic.
     * @return The title and name of this medic.
     */
    public String toString()
    {
        return getTitle() + " " + getName();
    }
}
