package cz.cvut.kbss.jopa.query.sparql;

import org.antlr.v4.runtime.TokenStreamRewriter;

/**
 * Allows plugging into the SPARQL query assembly process and modifying the resulting query.
 */
interface SparqlAssemblyModifier {

    /**
     * Modifies the query.
     *
     * @param tokenRewriter   Token stream rewriter used to modify the query
     * @param queryAttributes Query attributes
     */
    void modify(TokenStreamRewriter tokenRewriter, QueryAttributes queryAttributes);
}
