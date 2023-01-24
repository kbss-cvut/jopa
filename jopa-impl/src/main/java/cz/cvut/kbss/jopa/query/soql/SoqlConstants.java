package cz.cvut.kbss.jopa.query.soql;

/**
 * Constants of the Semantic Object Query Language (SOQL).
 */
class SoqlConstants {

    /**
     * {@code LIKE} operator.
     */
    static final String LIKE = "LIKE";

    /**
     * SPARQL shortcut for {@code rdf:type} - {@code a}.
     */
    static final String RDF_TYPE = "a";

    private SoqlConstants() {
        throw new AssertionError();
    }
}
