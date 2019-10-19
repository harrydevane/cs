
/**
 * The class Person. Represents a Person.
 *
 * @author Harry Devane
 * @version 2018.12.08
 */
public class Person
{
    // The person's full name
    private String name;
    // The person's title
    private String title;

    /**
     * Constructor for Person objects.
     * @param name The person's name.
     * @param title The person's title.
     */
    public Person(String name, String title)
    {
        this.name = name;
        this.title = title;
    }

    /**
     * Return the full name of this person.
     * @return The full name of this person.
     */
    public String getName()
    {
        return name;
    }

    /**
     * Return the person's title.
     * @return The title of this person.
     */
    public String getTitle()
    {
        return title;
    }

    /**
     * Changes the person's title to the specified title.
     * @param title The new title.
     */
    public void changeTitle(String title)
    {
        this.title = title;
    }
}
