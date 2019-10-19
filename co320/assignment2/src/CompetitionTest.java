import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import java.util.ArrayList;

/**
 * The test class CompetitionTest.
 *
 * @author  David J. Barnes
 * @version 2015.11.07
 */
public class CompetitionTest
{
    private Competition comp;
    private Personality personal2;
    private Personality personal3;
    private Personality personal4;
    private Personality personal5;
    private Personality personal6;

    /**
     * Default constructor for test class PersonalityListTest
     */
    public CompetitionTest()
    {
    }

    /**
     * Sets up the test fixture.
     *
     * Called before every test case method.
     */
    @Before
    public void setUp()
    {
        setupStandardData();
        personal2.increaseVotes(100);
        personal3.increaseVotes(98);
        personal4.increaseVotes(50);
        personal5.increaseVotes(50);
        personal6.increaseVotes(42);
    }

    /**
     * Create a new Competition and Personalities to go in it.
     * The Personalities are not added to the list.
     */
    private void setupStandardData()
    {
        comp = new Competition();
        personal2 = new Personality("Lewis Hamilton", "F1");
        personal3 = new Personality("Sarah Hunter", "Rugby");
        personal4 = new Personality("Moeen Ali", "Cricket");
        personal5 = new Personality("Katerina Johnson-Thompson", "Athletics");
        personal6 = new Personality("David Weir", "Cycling");
    }

    /**
     * Tears down the test fixture.
     *
     * Called after every test case method.
     */
    @After
    public void tearDown()
    {
    }

    @Test
    public void add()
    {
        // Basic test of addition.
        System.out.println("### Add one personality to an empty list.");
        comp.addPersonality(personal2);
        assertEquals(1, comp.getSize());
    }

    @Test
    public void size()
    {
        System.out.println("### Add several personalities to a list.");
        // Basic tests of the getSize method.
        assertEquals(0, comp.getSize());

        comp.addPersonality(personal2);
        assertEquals(1, comp.getSize());

        comp.addPersonality(personal3);
        assertEquals(2, comp.getSize());

        comp.addPersonality(personal4);
        comp.addPersonality(personal5);
        comp.addPersonality(personal6);
        assertEquals(5, comp.getSize());
    }

    @Test
    public void list()
    {
        // Visual checks needed to make sure this is correct.
        // Empty list.
        System.out.println("### Print an empty list.");
        comp.list();
        System.out.println();

        // One person.
        System.out.println("### Print a list of one person.");
        comp.addPersonality(personal2);
        comp.list();
        System.out.println();

        // Five people.
        System.out.println("### Print a list of five people.");
        comp.addPersonality(personal3);
        comp.addPersonality(personal4);
        comp.addPersonality(personal5);
        comp.addPersonality(personal6);
        comp.list();
        System.out.println();
    }

    @Test
    public void shortListEmpty()
    {
        // Shortlist empty list.
        System.out.println("### Test calling shortlist on an empty list.");
        comp.shortlist(0);
        assertEquals(0, comp.getSize());
    }

    @Test
    public void shortListNoOne()
    {
        // Remove no-one.
        System.out.println("### Test calling shortlist when no one should be removed.");
        setupAll();
        comp.shortlist(0);
        assertEquals(5, comp.getSize());
    }

    @Test
    public void shortListBoundary()
    {
        System.out.println("### Test calling shortlist when no one should be removed.");
        setupAll();
        // Remove no-one.
        comp.shortlist(42);
        assertEquals(5, comp.getSize());
    }

    @Test
    public void shortListRemoveOne()
    {
        System.out.println("### Test calling shortlist when one person should be removed.");
        setupAll();
        // Remove one.
        comp.shortlist(43);
        assertEquals(4, comp.getSize());
    }

    @Test
    public void shortListRemoveMultiple()
    {
        System.out.println("### Test calling shortlist when multiple people should be removed.");
        setupAll();
        // Remove multiple with the same number of votes..
        comp.shortlist(51);
        assertEquals(2, comp.getSize());
    }

    @Test
    public void shortListKeepOne()
    {
        System.out.println("### Test calling shortlist so only one person remains.");
        setupAll();
        // Keep just one.
        comp.shortlist(100);
        assertEquals(1, comp.getSize());
    }

    @Test
    public void shortListKeepNoOne()
    {
        System.out.println("### Test calling shortlist when everyone should be removed.");
        setupAll();
        // Keep no one.
        comp.shortlist(1000);
        assertEquals(0, comp.getSize());
    }

    /**
     * Put all of the personalities into the list.
     */
    private void setupAll()
    {
        comp.addPersonality(personal2);
        comp.addPersonality(personal3);
        comp.addPersonality(personal4);
        comp.addPersonality(personal5);
        comp.addPersonality(personal6);
    }

    /**
     * Determine whether the given list contains the given
     * personality.
     * @param list The list of personalities.
     * @param name The name sought.
     * @return true if present, false otherwise.
     */
    private boolean isInList(ArrayList<Personality> list,
                             String name)
    {
        boolean found = false;
        int index = 0;
        while(!found && index < list.size()) {
            found = name.equals(list.get(index).getName());
            index++;
        }
        return found;
    }

    @Test
    public void voteForPositive()
    {
        System.out.println("### Test voting for someone who exists.");
        setupAll();
        Personality person = personal6;
        int beforeVotes = person.getVotes();
        comp.voteFor(person.getName());
        assertEquals(beforeVotes + 1, person.getVotes());
    }

    @Test
    public void voteForNegative()
    {
        System.out.println("### Test voting for someone who does not exist.");
        System.out.println("### An error message should be printed.");
        setupAll();
        Personality person = personal6;
        int beforeVotes = person.getVotes();
        comp.voteFor(person.getName() + "?");
        System.out.println();
        assertEquals(beforeVotes, person.getVotes());
    }

    @Test
    public void voteForNegativeContains()
    {
        System.out.println("### Test voting for someone who does not exist.");
        System.out.println("### An error message should be printed.");
        setupAll();
        Personality person = personal2;
        int beforeVotes = person.getVotes();
        // Exact match is required for a name.
        comp.voteFor("a");
        System.out.println();
        assertEquals(beforeVotes, person.getVotes());
    }

    @Test
    public void getMost0()
    {
        System.out.println("### Test getMost with an empty list.");
        // Start with an empty list of personalities.
        ArrayList<Personality> result = comp.getMost(0);
        assertNotNull(result);
        assertEquals(0, result.size());
    }

    @Test
    public void getMost00()
    {
        System.out.println("### Test getMost with 0 parameter and list size 5.");
        // Now with some personalities.
        setupAll();
        int originalSize = comp.getSize();
        ArrayList<Personality> result = comp.getMost(0);
        assertNotNull(result);
        assertEquals(0, result.size());
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMost1()
    {
        System.out.println("### Test getMost with 1 parameter and list size 5.");
        setupAll();
        int originalSize = comp.getSize();
        ArrayList<Personality> result = comp.getMost(1);
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(isInList(result, "Lewis Hamilton"));
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMost2()
    {
        System.out.println("### Test getMost with 2 parameter and list size 5 and no ties.");
        setupAll();
        int originalSize = comp.getSize();
        ArrayList<Personality> result = comp.getMost(2);
        assertNotNull(result);
        assertEquals(2, result.size());
        assertTrue(isInList(result, "Lewis Hamilton"));
        assertTrue(isInList(result, "Sarah Hunter"));
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMost3Tied()
    {
        System.out.println("### Test getMost with 3 parameter and list size 5 and ties.");
        setupAll();
        int originalSize = comp.getSize();
        ArrayList<Personality> result = comp.getMost(3);
        assertNotNull(result);
        assertEquals(4, result.size());
        assertTrue(isInList(result, "Lewis Hamilton"));
        assertTrue(isInList(result, "Sarah Hunter"));
        assertTrue(isInList(result, "Moeen Ali"));
        assertTrue(isInList(result, "Katerina Johnson-Thompson"));
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMost3NotTied()
    {
        System.out.println("### Test getMost with 3 parameter and list size 5 with no ties.");
        setupAll();
        int originalSize = comp.getSize();
        personal4.increaseVotes(2);
        ArrayList<Personality> result = comp.getMost(3);
        assertNotNull(result);
        assertEquals(3, result.size());
        assertTrue(isInList(result, "Lewis Hamilton"));
        assertTrue(isInList(result, "Sarah Hunter"));
        assertTrue(isInList(result, "Moeen Ali"));
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMostAllExactly()
    {
        System.out.println("### Test getMost with parameter equal to list size.");
        setupAll();
        int originalSize = comp.getSize();

        int number = comp.getSize();

        ArrayList<Personality> result = comp.getMost(number);
        assertNotNull(result);
        // Check that the original list is unchanged.
        assertEquals(number, comp.getSize());
        // Check we have them all.
        assertEquals(number, result.size());
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMostAllMore()
    {
        System.out.println("### Test getMost with parameter one more than list size.");
        setupAll();
        int originalSize = comp.getSize();

        int number = comp.getSize();

        // Try to get more than available.
        ArrayList<Personality> result = comp.getMost(number + 1);
        assertNotNull(result);
        // Check that the original list is unchanged.
        assertEquals(number, comp.getSize());
        // Check we have them all.
        assertEquals(number, result.size());
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMostTwice()
    {
        System.out.println("### Test getMost twice with 2 parameter to check return list size.");
        setupAll();
        int originalSize = comp.getSize();

        ArrayList<Personality> result = comp.getMost(2);
        assertEquals(2, result.size());
        // Check that the previous results are not
        // retained.
        result = comp.getMost(1);
        assertEquals(1, result.size());
        assertEquals(comp.getSize(), originalSize);
    }

    @Test
    public void getMostAllSameVotes()
    {
        System.out.println("### Test getMost when all votes are the same.");
        // Start with a fresh set of test objects.
        setupStandardData();

        personal2.increaseVotes(100);
        personal3.increaseVotes(100);
        personal4.increaseVotes(100);
        personal5.increaseVotes(100);
        personal6.increaseVotes(100);

        setupAll();

        int originalSize = comp.getSize();
        for(int most = 1; most <= originalSize + 1; most++) {
            ArrayList<Personality> result = comp.getMost(most);
            assertEquals(originalSize, result.size());
        }

    }
}





