package cz.cvut.kbss.jopa.vocabulary;

/**
 * A subset of the OWL vocabulary.
 */
public class OWL {

    /**
     * OWL vocabulary namespace.
     */
    public static final String NAMESPACE = "http://www.w3.org/2002/07/owl#";

    /**
     * Typical prefix used for {@link #NAMESPACE}.
     */
    public static final String PREFIX = "owl";

    /**
     * {@code owl:Class} class.
     */
    public static final String CLASS = NAMESPACE + "Class";

    /**
     * {@code owl:Thing}
     */
    public static final String THING = NAMESPACE + "Thing";

    /**
     * {@code owl:Nothing}
     */
    public static final String NOTHING = NAMESPACE + "Nothing";

    /**
     * {@code owl:minCardinality}
     */
    public static final String MIN_CARDINALITY = NAMESPACE + "minCardinality";

    /**
     * {@code owl:maxCardinality}
     */
    public static final String MAX_CARDINALITY = NAMESPACE + "maxCardinality";

    /**
     * {@code owl:sameAs}
     */
    public static final String SAME_AS = NAMESPACE + "sameAs";

    /**
     * {@code owl:AnnotationProperty}
     */
    public static final String ANNOTATION_PROPERTY = NAMESPACE + "AnnotationProperty";

    /**
     * {@code owl:DatatypeProperty}
     */
    public static final String DATATYPE_PROPERTY = NAMESPACE + "DatatypeProperty";

    /**
     * {@code owl:ObjectProperty}
     */
    public static final String OBJECT_PROPERTY = NAMESPACE + "ObjectProperty";

    /**
     * {@code owl:Ontology}
     */
    public static final String ONTOLOGY = NAMESPACE + "Ontology";

    private OWL() {
        throw new AssertionError();
    }
}
