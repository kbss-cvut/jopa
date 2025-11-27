package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenQueryParameter;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;

/**
 * Optimizes entity loading by modifying the query to fetch all entity attributes.
 * <p>
 * This optimizer is applicable for SELECT queries that select instances of an entity class. Instead of loading the
 * instances one by one after the query is evaluated, this optimizer modifies the query to fetch all available entity
 * attributes by injecting a triple pattern with unbound predicate and object variables into the query.
 */
public class UnboundPredicateObjectSparqlAssemblyModifier implements SparqlAssemblyModifier {

    @Override
    public void modify(TokenStreamSparqlQueryHolder queryHolder, TokenStreamRewriter tokenRewriter,
                       QueryAttributes queryAttributes) {
        assert queryAttributes.queryType() == QueryType.SELECT;
        assert queryHolder.getProjectedQueryParameters().size() == 1;

        final TokenQueryParameter<?> p = queryHolder.getProjectedQueryParameters().get(0);
        tokenRewriter.insertAfter(p.getSingleToken(), generateProjectionModification(p));
        tokenRewriter.insertBefore(queryAttributes.lastClosingCurlyBraceToken(), generateSelectionTriplePattern(p, tokenRewriter.getTokenStream(), queryAttributes.lastClosingCurlyBraceToken()));
    }

    private static String generateProjectionModification(TokenQueryParameter<?> subjectParam) {
        final String baseName = getBaseParamName(subjectParam);
        return " " + property(baseName) + " " + value(baseName);
    }

    static String getBaseParamName(QueryParameter<?> p) {
        return p.getName() != null ? p.getName() : p.getPosition().toString();
    }

    private static String property(String baseName) {
        return "?" + baseName + "P";
    }

    private static String value(String baseName) {
        return "?" + baseName + "V";
    }

    private static String generateSelectionTriplePattern(TokenQueryParameter<?> subjectParam, TokenStream tokenStream,
                                                         Token lastClosingCurlyBracket) {
        final String baseName;
        final char varPrefix;
        if (subjectParam.getPosition() != null) {
            baseName = subjectParam.getPosition().toString();
            varPrefix = '$';
        } else {
            baseName = subjectParam.getName();
            varPrefix = '?';
        }
        return (requiresDot(tokenStream, lastClosingCurlyBracket) ? " . " : "")
                + varPrefix + baseName + " " + property(baseName) + " " + value(baseName) + " . ";
    }

    private static boolean requiresDot(TokenStream tokenStream, Token lastClosingCurlyBracket) {
        int position = lastClosingCurlyBracket.getTokenIndex() - 1;
        while (position > 0) {
            final Token t = tokenStream.get(position--);
            if (!t.getText().isBlank()) {
                return !".".equals(t.getText());
            }
        }
        return true;
    }
}
