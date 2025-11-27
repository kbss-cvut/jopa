package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenQueryParameter;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import org.antlr.v4.runtime.TokenStreamRewriter;

import java.util.ArrayList;
import java.util.List;

/**
 * Optimizes entity loading by modifying the query to fetch all named attributes.
 * <p>
 * This optimizer is applicable for SELECT queries that select instances of an entity class. Instead of loading the
 * instances one by one after the query is evaluated, this optimizer modifies the query to fetch all available entity
 * attributes by injecting optional triple patterns for each of the entity attributes.
 * <p>
 * When not to use this modifies:
 * <ul>
 *     <li>When the result type has {@literal Properties} field</li>
 *     <li>When the result type has subclasses</li>
 * </ul>
 */
public class AttributeEnumeratingSparqlAssemblyModifier implements SparqlAssemblyModifier {

    private final EntityType<?> resultType;

    public AttributeEnumeratingSparqlAssemblyModifier(EntityType<?> resultType) {
        this.resultType = resultType;
        assert resultType.getProperties() == null;
    }

    @Override
    public void modify(TokenStreamSparqlQueryHolder queryHolder, TokenStreamRewriter tokenRewriter,
                       QueryAttributes queryAttributes) {
        assert queryAttributes.queryType() == QueryType.SELECT;
        assert queryHolder.getProjectedQueryParameters().size() == 1;

        final TokenQueryParameter<?> p = queryHolder.getProjectedQueryParameters().get(0);
        final List<String> variablesToProject = addAttributeSelection(queryHolder, tokenRewriter, queryAttributes);
        tokenRewriter.insertAfter(p.getSingleToken(), " " + String.join(" ", variablesToProject));
    }

    private List<String> addAttributeSelection(TokenStreamSparqlQueryHolder queryHolder,
                                               TokenStreamRewriter tokenRewriter,
                                               QueryAttributes queryAttributes) {
        final StringBuilder attributePatterns = new StringBuilder();
        final List<String> variables = new ArrayList<>();
        final String subjectParamName = UnboundPredicateObjectSparqlAssemblyModifier.getBaseParamName(queryHolder.getProjectedQueryParameters()
                                                                                                                 .get(0));
        final String subjectVariable = "?" + subjectParamName;
        resultType.getAttributes().forEach(att -> {
            final String variable = "?" + subjectParamName + att.getName();
            variables.add(variable);
            attributePatterns.append("OPTIONAL { ").append(subjectVariable).append(" <").append(att.getIRI())
                             .append("> ").append(variable).append(" } ");
        });
        if (resultType.getTypes() != null) {
            final String variable = "?" + subjectParamName + "types";
            variables.add(variable);
            attributePatterns.append("OPTIONAL { ").append(subjectVariable).append(" a ").append(variable)
                             .append(" } ");
        }
        tokenRewriter.insertBefore(queryAttributes.lastClosingCurlyBraceToken(), attributePatterns.toString());
        return variables;
    }
}
