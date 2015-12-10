package cz.cvut.kbss.jopa.accessors;


public class DataSourceCreationException extends RuntimeException {
    public DataSourceCreationException() {
    }

    public DataSourceCreationException(String message) {
        super(message);
    }

    public DataSourceCreationException(String message, Throwable cause) {
        super(message, cause);
    }

    public DataSourceCreationException(Throwable cause) {
        super(cause);
    }
}
