/**
 * The class CarPark. Represents a car park at a location with a
 * capacity and an occupancy.
 */
public class CarPark {

    private final String location;
    private int capacity;
    private int occupancy;

    /**
     * Constructor for CarPark objects.
     *
     * @param location The location of the car park.
     * @param capacity The capacity of the car park.
     */
    public CarPark(String location, int capacity) {
        this.location = location;
        this.capacity = capacity;
    }

    /**
     * Gets the location of the car park.
     *
     * @return The location of the car park.
     */
    public String getLocation() {
        return location;
    }

    /**
     * Gets the capacity of the car park.
     *
     * @return The capacity of the cark park.
     */
    public int getCapacity() {
        return capacity;
    }

    /**
     * Gets the current occupancy of the car park.
     *
     * @return The current occupancy of the car park.
     */
    public int getOccupancy() {
        return occupancy;
    }

    /**
     * Parks a car in the car park if there is space for it.
     */
    public void park() {
        if (occupancy < capacity) {
            occupancy++;
        } else {
            System.out.println("The car park is full.");
        }
    }

    /**
     * Removes a car from the car park if it is not empty.
     */
    public void leave() {
        if (occupancy > 0) {
            occupancy--;
        } else {
            System.out.println("The car park is empty.");
        }
    }

    /**
     * Changes the cark park capacity by the provided amount.
     *
     * @param amount The amount to change the capacity by.
     */
    public void changeCapacity(int amount) {
        if (occupancy > 0 && (capacity + amount) < occupancy) {
            System.out.println("The capacity can't be changed lower than the current occupancy.");
        } else if ((capacity + amount) < 0) {
            capacity = 0;
            System.out.println("The car park is now closed.");
        } else {
            capacity += amount;
        }
    }

    /**
     * Prints the details of the car park to the console.
     */
    public void printDetails() {
        System.out.println(location + " car park has " + (capacity - occupancy) + " spaces.");
    }
}
