package cz.cvut.kbss.ontodriver.owlapi.exception;

public class BindingValueMismatchException extends RuntimeException {

    public BindingValueMismatchException() {
    }

    public BindingValueMismatchException(String message) {
        super(message);
    }

    public BindingValueMismatchException(String message, Throwable cause) {
        super(message, cause);
    }

    public BindingValueMismatchException(Throwable cause) {
        super(cause);
    }
}
