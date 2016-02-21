package cz.cvut.kbss.jopa.exception;

/**
 * Signals that an entity field mapping is not valid.
 * <p/>
 * It can mean for example that its type does not correspond to the mapping (singular field for plural mapping,
 * non-Set for a {@link cz.cvut.kbss.jopa.model.annotations.Types} field) etc.
 */
public class InvalidFieldMappingException extends MetamodelInitializationException {

    public InvalidFieldMappingException() {
    }

    public InvalidFieldMappingException(String message) {
        super(message);
    }
}
