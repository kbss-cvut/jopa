package cz.cvut.kbss.jopa.query.sparql;

/**
 * Constants of SPARQL.
 */
public class SparqlConstants {

    /**
     * The {@literal SELECT} keyword.
     */
    public static final String SELECT = "SELECT";

    /**
     * The {@literal WHERE} keyword.
     */
    public static final String WHERE = "WHERE";

    /**
     * The {@literal a} keyword representing the rdf:type IRI.
     */
    public static final String RDF_TYPE_SHORTCUT = "a";

    private SparqlConstants() {
        throw new AssertionError();
    }
}
