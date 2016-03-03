package cz.cvut.kbss.ontodriver.owlapi.exception;

/**
 * Marks issues with parsing of IRI mapping files.
 */
public class MappingFileParserException extends RuntimeException {

    public MappingFileParserException(String message) {
        super(message);
    }

    public MappingFileParserException(String message, Throwable cause) {
        super(message, cause);
    }

    public MappingFileParserException(Throwable cause) {
        super(cause);
    }
}
