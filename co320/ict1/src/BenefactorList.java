import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * The class BenefactorList. Stores a list of Benefactors
 * who have contributed to a charity.
 *
 * @author Harry Devane
 * @version 27.11.2018
 */
public class BenefactorList {

    private final List<Benefactor> benefactors = new LinkedList<>();

    /**
     * Adds a benefactor to the list.
     *
     * @param benefactor The benefactor to add.
     */
    public void addBenefactor(Benefactor benefactor) {
        benefactors.add(benefactor);
    }

    /**
     * Returns the number of benefactors are in the list.
     *
     * @return The number of benefactors in the list.
     */
    public int howMany() {
        return benefactors.size();
    }

    /**
     * Prints the details of benefactors to the console.
     */
    public void list() {
        for (Benefactor benefactor : benefactors) {
            System.out.println(benefactor.getDetails());
        }
    }

    /**
     * Returns if the list contains a benefactor with the
     * provided name or not.
     *
     * @param name The name to search the list for.
     * @return true if the list contains a benefactor with the provided name.
     */
    public boolean isABenefactor(String name) {
        for (int i = 0; i < benefactors.size(); i++) {
            Benefactor benefactor = benefactors.get(i);
            if (benefactor.getName().equalsIgnoreCase(name)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Removes all benefactors who have Gift Aided their donations
     * and returns them in a list.
     *
     * @return A list of the benefactors removed.
     */
    public List<Benefactor> removeGiftAiders() {
        List<Benefactor> results = new LinkedList<>();
        Iterator<Benefactor> iterator = benefactors.iterator();
        while (iterator.hasNext()) {
            Benefactor benefactor = iterator.next();
            if (benefactor.isGiftAid()) {
                iterator.remove();
                results.add(benefactor);
            }
        }
        return results;
    }
}
