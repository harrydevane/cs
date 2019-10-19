import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * The class Competition. Represents a sports competition.
 *
 * @author Harry Devane
 * @version 2018.11.17
 */
public class Competition {

    private final List<Personality> personalities = new ArrayList<>();

    /**
     * Adds a personality to the competition.
     *
     * @param personality The personality to add.
     */
    public void addPersonality(Personality personality) {
        personalities.add(personality);
    }

    /**
     * Gets the number of personalities in this competition.
     *
     * @return The number of personalities.
     */
    public int getSize() {
        return personalities.size();
    }

    /**
     * Prints details of all personalities in this competition.
     */
    public void list() {
        for (Personality personality : personalities) {
            System.out.println(personality.getDetails());
        }
    }

    /**
     * Finds a personality with a name and increases it's votes by 1.
     *
     * @param name The name of the personality to increase votes for.
     */
    public void voteFor(String name) {
        for (Personality personality : personalities) {
            if (personality.getName().equals(name)) {
                personality.increaseVotes(1);
                return;
            }
        }
    }

    /**
     * Removes all personalities with fewer votes than the specified
     * minimum votes parameter.
     *
     * @param minimumVotes The minimum votes a personality must have to be retained.
     */
    public void shortlist(int minimumVotes) {
        Iterator<Personality> iterator = personalities.iterator();
        while (iterator.hasNext()) {
            Personality personality = iterator.next();
            if (personality.getVotes() < minimumVotes) {
                iterator.remove();
            }
        }
    }

    /**
     * Returns a specified amount of personalities with the most votes.
     *
     * @param amount The amount of personalities to return.
     * @return The list of personalities.
     */
    public List<Personality> getMost(int amount) {
        if (amount <= 0) {
            return Collections.emptyList();
        }
        if (amount >= personalities.size()) {
            return new ArrayList<>(personalities);
        }
        List<Personality> results = new ArrayList<>();
        Collections.sort(personalities, (personality1, personality2) -> personality2.getVotes() - personality1.getVotes());
        for (int i = 0; i < amount; i++) {
            results.add(personalities.get(i));
        }
        int lastVotes = results.get(amount - 1).getVotes();
        for (int i = amount; i < personalities.size(); i++) {
            Personality personality = personalities.get(i);
            if (personality.getVotes() != lastVotes) {
                break;
            }
            results.add(personality);
        }
        return results;
    }
}
