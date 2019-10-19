/**
 * A class storing details of a doctor.
 *
 * @author David J. Barnes
 * @version 2018.12.04
 */
public class Doctor extends Medic
{

    /**
     * Set the doctor's name and title.
     * @param name The doctor's name.
     */
    public Doctor(String name)
    {
        super(name, "Dr");
    }
}
