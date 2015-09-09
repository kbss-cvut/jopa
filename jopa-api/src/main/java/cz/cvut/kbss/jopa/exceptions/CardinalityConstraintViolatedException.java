package cz.cvut.kbss.jopa.exceptions;

/**
 * This exception is thrown when cardinality restriction constraint is violated.
 * <p/>
 * For example when the model expects only single value of a property but multiple are present.
 * <p/>
 */
public class CardinalityConstraintViolatedException extends IntegrityConstraintViolatedException {

    public CardinalityConstraintViolatedException() {
    }

    public CardinalityConstraintViolatedException(String message, Throwable cause) {
        super(message, cause);
    }

    public CardinalityConstraintViolatedException(String message) {
        super(message);
    }

    public CardinalityConstraintViolatedException(Throwable cause) {
        super(cause);
    }
}
