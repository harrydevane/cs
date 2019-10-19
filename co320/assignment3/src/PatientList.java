import java.util.*;

/**
 * The PatientList class represents a set of patients waiting
 * to been seen.
 *
 * @author David J. Barnes
 * @version 2018.12.04
 */
public class PatientList
{
    // The patients' medic.
    private Medic medic;
    // Where the consultations take place.
    private String room;
    // Patients to be seen.
    private List<Patient> patients;
    // A limit on the number of patients in the list.
    private int capacity;

    /**
     * Create a PatientList with a maximum size.
     * All other details are set to default values.
     */
    public PatientList(int maxNumberOfPatients)
    {
        medic = null;
        room = "unknown";
        patients = new ArrayList<Patient>();
        capacity = maxNumberOfPatients;
    }

    /**
     * Enrol a patient in this PatientList.
     * A patient can only be enrolled if the list is not already full.
     * An error message is printed if an attempt is made to enrolled a
     * patient to a full list.
     * @param patient The patient to be enrolled.
     */
    public void enroll(Patient patient)
    {
        if(patients.size() == capacity) {
            System.out.print("The list is full. ");
            System.out.println("No further patients can be enrolled.");
        }
        else {
            patients.add(patient);
        }
    }

    /**
     * Return the number of patients who have been enrolled.
     * @return The number of patients who have been enrolled.
     */
    public int numberOfPatients()
    {
        return patients.size();
    }

    /**
     * Return the capacity of this list.
     * @return The capacity of this PatientList.
     */
    public int getCapacity()
    {
        return capacity;
    }

    /**
     * Return this list's medic.
     * @return The medic for this list.
     */
    public Medic getMedic()
    {
        return medic;
    }

    /**
     * Set the room details.
     * @param roomNumber The room.
     */
    public void setRoom(String roomNumber)
    {
        room = roomNumber;
    }

    /**
     * Set the medic for this PatientList.
     * @param medic The list's medic.
     */
    public void setMedic(Medic medic)
    {
        this.medic = medic;
    }

    /**
     * Print out details of this list to the standard
     * output.
     */
    public void printList()
    {
        if(medic == null) {
            System.out.println("Patients for: unknown room: " + room);
        }
        else {
            System.out.println("Patients for: " + medic.toString() +
                           " room: " + room);
        }
        System.out.println("Number of patients: " + numberOfPatients());
        System.out.println("Patient list:");
        for(Patient patient : patients) {
            System.out.print("    ");
            System.out.println(patient.toString());
        }
    }
}
