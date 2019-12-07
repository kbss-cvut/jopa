package cz.cvut.kbss.jopa.query.soql;


public class MyPair<U, V>
{
    private final U first;   	// first field of a Pair
    private final V second;  	// second field of a Pair

    // Constructs a new Pair with specified values
    private MyPair(U first, V second)
    {
        this.first = first;
        this.second = second;
    }

    public boolean equals(Object o)
    {
        if (this == o)
            return true;

        if (o == null || getClass() != o.getClass())
            return false;

        MyPair<?, ?> pair = (MyPair<?, ?>) o;

        if (!first.equals(pair.first))
            return false;
        return second.equals(pair.second);
    }

    public int hashCode() {
        return 31 * first.hashCode() + second.hashCode();
    }

    public String toString()
    {
        return "(" + first + ", " + second + ")";
    }

    public String toQueryParam()
    {
        return first + toUc(second.toString());
    }

    private String toUc(String s){
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    public U getFirst() {
        return first;
    }

    public V getSecond() {
        return second;
    }

    public boolean isSecondEqual(V b){
        return second.equals(b);
    }

    public static <U, V> MyPair <U, V> of(U a, V b)
    {
        return new MyPair<>(a, b);
    }
}
