package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryType;
import org.antlr.v4.runtime.TokenStreamRewriter;

/**
 * Optimizes entity loading by modifying the query to fetch all entity attributes.
 * <p>
 * This optimizer is applicable for SELECT queries that select instances of an entity class. Instead of loading the
 * instances one by one after the query is evaluated, this optimizer modifies the query to fetch all available entity
 * attributes.
 */
public class EntityLoadingSparqlAssemblyModifier implements SparqlAssemblyModifier {

    @Override
    public void modify(TokenStreamSparqlQueryHolder queryHolder, TokenStreamRewriter tokenRewriter,
                       QueryAttributes queryAttributes) {
        assert queryAttributes.queryType() == QueryType.SELECT;
        assert queryHolder.getProjectedQueryParameters().size() == 1;

        final TokenQueryParameter<?> p = queryHolder.getProjectedQueryParameters().get(0);
        tokenRewriter.insertAfter(p.getTokens().get(0), generateProjectionModification(p));
        tokenRewriter.insertBefore(queryAttributes.lastClosingCurlyBraceToken(), generateSelectionTriplePattern(p));
    }

    private String generateProjectionModification(TokenQueryParameter<?> subjectParam) {
        final String baseName = getBaseParamName(subjectParam);
        return " " + property(baseName) + " " + value(baseName);
    }

    private static String getBaseParamName(QueryParameter<?> p) {
        return p.getName() != null ? p.getName() : p.getPosition().toString();
    }

    private static String property(String baseName) {
        return "?" + baseName + "P";
    }

    private static String value(String baseName) {
        return "?" + baseName + "V";
    }

    private String generateSelectionTriplePattern(TokenQueryParameter<?> subjectParam) {
        final String baseName;
        final char varPrefix;
        if (subjectParam.getPosition() != null) {
            baseName = subjectParam.getPosition().toString();
            varPrefix = '$';
        } else {
            baseName = subjectParam.getName();
            varPrefix = '?';
        }
        return varPrefix + baseName + " " + property(baseName) + " " + value(baseName) + " . ";
    }
}
