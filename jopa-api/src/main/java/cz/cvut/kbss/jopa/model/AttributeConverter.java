package cz.cvut.kbss.jopa.model;

/**
 * A class that implements this interface can be used to convert entity attribute state into axiom/triple value
 * representation and back again.
 * <p>
 * Note that the X and Y types may be the same Java type.
 *
 * @param <X> the type of the entity attribute
 * @param <Y> the type of the axiom/triple value
 */
public interface AttributeConverter<X, Y> {

    /**
     * Converts the value stored in the entity attribute into the data representation to be stored in the repository and
     * supported by the OntoDriver API.
     *
     * @param value Value to convert
     * @return Converted data
     */
    Y convertToAxiomValue(X value);

    /**
     * Converts the data stored in the repository into the value to be stored in the entity attribute.
     * <p>
     * Note that it is the responsibility of the converter writer to specify the correct {@code value} type for the
     * corresponding value for use by the OntoDriver: i.e., persistence providers are not expected to do such type
     * conversion.
     *
     * @param value Value to convert
     * @return Converted data
     */
    X convertToAttribute(Y value);
}
