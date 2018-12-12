package cz.cvut.kbss.jopa.vocabulary;

/**
 * A subset of the RDF vocabulary.
 */
public class RDF {

    /**
     * RDF vocabulary namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    /**
     * Typical prefix used for {@link #NAMESPACE}.
     */
    public static final String PREFIX = "rdf";

    /**
     * The {@code rdf:type} property indicates that a resource is a member of a class.
     */
    public static final String TYPE = NAMESPACE + "type";

    /**
     * The {@code rdf:value} property identifies the principal value (usually a string) of a property when the property
     * value is a structured resource.
     */
    public static final String VALUE = NAMESPACE + "value";

    /**
     * The {@code rdf:Statement} class represents statements about the properties of resources.
     *
     * @see #SUBJECT
     * @see #PREDICATE
     * @see #OBJECT
     */
    public static final String STATEMENT = NAMESPACE + "Statement";

    /**
     * The subject of an RDF statement.
     */
    public static final String SUBJECT = NAMESPACE + "subject";

    /**
     * The predicate of an RDF statement.
     */
    public static final String PREDICATE = NAMESPACE + "predicate";

    /**
     * The predicate of an RDF statement.
     */
    public static final String OBJECT = NAMESPACE + "object";

    /**
     * {@code rdf:Property} represents those resources that are RDF properties.
     */
    public static final String PROPERTY = NAMESPACE + "Property";

    private RDF() {
        throw new AssertionError();
    }
}
