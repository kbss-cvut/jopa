package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import org.antlr.v4.runtime.TokenStreamRewriter;

class NoopSparqlAssemblyModifier implements SparqlAssemblyModifier {
    @Override
    public void modify(TokenStreamSparqlQueryHolder queryHolder, TokenStreamRewriter tokenRewriter,
                       QueryAttributes queryAttributes) {
        // Do nothing
    }
}
