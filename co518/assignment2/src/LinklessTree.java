
/**
 * Trees without explicit links.
 * Notice that various fields/methods have the protected modifier
 * when normally they would/should be private.
 * The reason is that this supports whitebox testing.
 *
 * @author Stefan Kahrs
 * @version 1
 */
//note the constraint on A is a slight generalisation of A extends Comparable<A>
//and is generally recommended when one wants a comparison operation
//it basically allows that the comparison op is implemented at a supertype
//of A, instead of A itself;
//for the assessment itself it makes no discernable difference
public class LinklessTree<A extends Comparable<? super A>>
{
    //sizes of subtrees at that node index
    protected int[] sizes;
    protected Object[] elems;
    //for annoying technical reason this has to be an array of objects

    /**
     * Constructor for objects of class LinklessTree
     */
    private static final int STARTSIZE=15;
    public LinklessTree()
    {
        assert STARTSIZE>0;
        elems = freshElemArray(STARTSIZE);
        sizes = new int[STARTSIZE];
        sizes[0]=0;
    }

    //size of whole tree is the size of the subtree rooted at 0
    public int size() {
        return getSize(0);
    }

    public A getValue(int index) {
        return (A)elems[index];
    }

    //auxiliary methods to index the arrays out of bounds too
    //they may help to reduce case distinctions
    protected A getKey(int subtree) {
        if (subtree>=elems.length) return null; // out of bounds
        return getValue(subtree);
    }

    protected int getSize(int subtree) {
        if (subtree>=elems.length) return 0; // out of bounds
        return sizes[subtree];
    }

    //encapsulates the cast on the allocation
    protected Object[] freshElemArray(int capacity) {
        return new Object[capacity];
    }

    //remainder needs to be modified

    //find index position of val in tree, if there, or where it goes, if not there
    protected int findIndex(A val) {
        int index = 0;
        A elem;
        while (index < elems.length && (elem = (A) elems[index]) != null) {
            int value = val.compareTo(elem);
            if (value == 0) {
                return index;
            }
            index = (index * 2) + (value < 0 ? 1 : 2);
        }
        return index;
    }

    //is value in tree
    public boolean contains(A val) {
        int index = 0;
        A elem;
        while (index < elems.length && (elem = (A) elems[index]) != null) {
            int value = val.compareTo(elem);
            if (value == 0) {
                return true;
            }
            index = (index * 2) + (value < 0 ? 1 : 2);
        }
        return false;
    }

    //grow the space in which we can palce the tree, so that at least one insertion will succeed
    protected void grow() {
        Object[] newElems = freshElemArray(elems.length * 2);
        int[] newSizes = new int[sizes.length * 2];
        System.arraycopy(elems, 0, newElems, 0, elems.length);
        System.arraycopy(sizes, 0, newSizes, 0, sizes.length);
        elems = newElems;
        sizes = newSizes;
    }

    //fetch the i-th element, in comparsion order
    public A get(int i){
        if (i < 0 || i >= size()) {
            throw new IndexOutOfBoundsException();
        }
        int index = 0;
        while (index < elems.length) {
            int leftSize = getSize((index * 2) + 1);
            if (i == leftSize) {
                return (A) elems[index];
            } else if (i > leftSize) {
                i -= (leftSize + 1);
                index = (index * 2) + 2;
            } else {
                index = (index * 2) + 1;
            }
        }
        return null;
    }

    //add x to tree, return true if tree was modified
    //we do not allow multiple copies of the equal objects in tree
    //equality is decided by using compareTo
    public boolean insert(A x){
        int index = 0;
        A elem;
        while (index < elems.length && (elem = (A) elems[index]) != null) {
            int value = x.compareTo(elem);
            if (value == 0) {
                return false; // duplicate
            }
            index = (index * 2) + (value < 0 ? 1 : 2);
        }
        while (index >= elems.length) {
            grow();
        }
        elems[index] = x;
        modifySizes(0, index, x, 1);
        return true;
    }

    //remove x from tree, return true if tree was modified
    public boolean delete(A x){
        int index = 0;
        A elem;
        while (index < elems.length && (elem = (A) elems[index]) != null) {
            int value = x.compareTo(elem);
            if (value == 0) {
                int leftSize = sizes[(index * 2) + 1];
                int rightSize = sizes[(index * 2) + 2];
                modifySizes(0, index, x, -1);
                if (leftSize > 0 && leftSize >= rightSize) {
                    elems[index] = deleteLargest((index * 2) + 1);
                } else if (rightSize > leftSize) {
                    elems[index] = deleteSmallest((index * 2) + 2);
                } else {
                    elems[index] = null;
                }
                return true;
            }
            index = (index * 2) + (value < 0 ? 1 : 2);
        }
        return false;
    }

    //not requested, but these might be useful auxiliary ops for delete
    private A deleteLargest(int subtree) {
        A elem = (A) elems[subtree];
        sizes[subtree] = sizes[subtree] - 1;
        if (elems.length > (subtree * 2) + 2) {
            if (elems[(subtree * 2) + 2] != null) {
                return deleteLargest((subtree * 2) + 2);
            }
        }
        elems[subtree] = null;
        return elem;
    }

    private A deleteSmallest(int subtree) {
        A elem = (A) elems[subtree];
        sizes[subtree] = sizes[subtree] - 1;
        if (elems.length > (subtree * 2) + 1) {
            if (elems[(subtree * 2) + 1] != null) {
                return deleteSmallest((subtree * 2) + 1);
            }
        }
        elems[subtree] = null;
        return elem;
    }

    private void modifySizes(int startIndex, int endIndex, A x, int diff) {
        int index = startIndex;
        A elem;
        while (index <= endIndex && (elem = (A) elems[index]) != null) {
            sizes[index] = sizes[index] + diff;
            int value = x.compareTo(elem);
            if (value == 0) {
                break;
            }
            index = (index * 2) + (value < 0 ? 1 : 2);
        }
    }
}
