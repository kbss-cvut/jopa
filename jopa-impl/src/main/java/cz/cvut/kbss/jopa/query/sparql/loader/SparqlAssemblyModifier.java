package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import org.antlr.v4.runtime.TokenStreamRewriter;

/**
 * Allows plugging into the SPARQL query assembly process and modifying the resulting query.
 */
public interface SparqlAssemblyModifier {

    /**
     * Modifies the query.
     *
     * @param queryHolder     Query holder
     * @param tokenRewriter   Token stream rewriter used to modify the query
     * @param queryAttributes Query attributes
     */
    void modify(TokenStreamSparqlQueryHolder queryHolder, TokenStreamRewriter tokenRewriter,
                QueryAttributes queryAttributes);
}
