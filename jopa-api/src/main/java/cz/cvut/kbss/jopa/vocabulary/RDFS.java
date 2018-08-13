package cz.cvut.kbss.jopa.vocabulary;

/**
 * A subset of the RDFS vocabulary.
 */
public class RDFS {

    /**
     * RDFS vocabulary namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/2000/01/rdf-schema#";

    /**
     * Typical prefix used for {@link #NAMESPACE}.
     */
    public static final String PREFIX = "rdfs";

    /**
     * The {@code rdfs:label} property is used to provide a human-readable version of a resource's name.
     */
    public static final String LABEL = NAMESPACE + "label";

    /**
     * The {@code rdfs:comment} property is used to provide a human-readable description of a resource.
     */
    public static final String COMMENT = NAMESPACE + "comment";

    /**
     * This corresponds to the generic concept of a type or category of resource.
     */
    public static final String CLASS = NAMESPACE + "Class";

    /**
     * An instance of {@code rdf:Property} that is used to indicate the class(es) that will have as members any resource
     * that has the indicated property.
     */
    public static final String DOMAIN = NAMESPACE + "domain";

    /**
     * An instance of {@code rdf:Property} that is used to indicate the class(es) that the values of a property will be
     * members of.
     */
    public static final String RANGE = NAMESPACE + "range";

    /**
     * All things described by RDF are called resources, and are members of the class {@code rdfs:Resource}.
     */
    public static final String RESOURCE = NAMESPACE + "Resource";

    /**
     * The class {@code rdfs:Literal} represents the self-denoting nodes called the 'literals' in the RDF graph
     * structure.
     * <p>
     * Property values such as textual strings are examples of RDF literals.
     */
    public static final String LITERAL = NAMESPACE + "Literal";

    /**
     * The {@code rdfs:subClassOf} property represents a specialization relationship between classes of resources.
     */
    public static final String SUB_CLASS_OF = NAMESPACE + "subClassOf";

    /**
     * The property {@code rdfs:subPropertyOf} is an instance of {@code rdf:Property} that is used to specify that one
     * property is a specialization of another.
     */
    public static final String SUB_PROPERTY_OF = NAMESPACE + "subPropertyOf";

    private RDFS() {
        throw new AssertionError();
    }
}
