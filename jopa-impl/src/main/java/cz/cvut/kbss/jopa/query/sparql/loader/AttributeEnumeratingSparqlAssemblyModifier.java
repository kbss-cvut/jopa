package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenQueryParameter;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import org.antlr.v4.runtime.TokenStreamRewriter;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * Optimizes entity loading by modifying the query to fetch all named attributes.
 * <p>
 * This optimizer is applicable for SELECT queries that select instances of an entity class. Instead of loading the
 * instances one by one after the query is evaluated, this optimizer modifies the query to fetch all available entity
 * attributes by injecting optional triple patterns for each of the entity attributes and projecting the values from
 * the query. If the result type has subclasses, all attributes of the subclasses are enumerated in the query so that
 * the instance loading can then determine the result type and load the appropriate instance with all the relevant
 * attribute data.
 * <p>
 * The injected patterns look like this:
 * <pre>
 * OPTIONAL { ?subject <property> ?value }
 * </pre>
 * If a descriptor with at most one context (for each attribute) is provided, the injected patterns look like this:
 * <pre>
 * OPTIONAL { GRAPH ?g { ?subject <property> ?value } }
 * </pre>
 * Where {@literal ?g} is determined from the descriptor.
 * <p>
 * When <b>not</b> to use this modifier:
 * <ul>
 *     <li>When the result type has {@literal Properties} field</li>
 * </ul>
 */
public class AttributeEnumeratingSparqlAssemblyModifier implements SparqlAssemblyModifier {

    private final IdentifiableEntityType<?> resultType;

    private final Descriptor descriptor;

    public AttributeEnumeratingSparqlAssemblyModifier(IdentifiableEntityType<?> resultType, Descriptor descriptor) {
        this.resultType = resultType;
        this.descriptor = descriptor;
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
        attributes().forEach(att -> {
            final String variable = "?" + subjectParamName + att.getName();
            variables.add(variable);
            attributePatterns.append("OPTIONAL { ");
            final Optional<String> ctx = context(att);
            ctx.ifPresent(uri -> attributePatterns.append("GRAPH <").append(uri).append("> { "));
            attributePatterns.append(subjectVariable).append(" <").append(att.getIRI())
                             .append("> ").append(variable).append(" } ");
            ctx.ifPresent(uri -> attributePatterns.append("} "));
        });
        if (hasTypes()) {
            final String variable = "?" + subjectParamName + "types";
            variables.add(variable);
            attributePatterns.append("OPTIONAL { ");
            final Optional<String> ctx = context(resultType.getTypes());
            ctx.ifPresent(uri -> attributePatterns.append("GRAPH <").append(uri).append("> { "));
            attributePatterns.append(subjectVariable).append(" a ").append(variable)
                             .append(" } ");
            ctx.ifPresent(uri -> attributePatterns.append("} "));
        }
        tokenRewriter.insertBefore(queryAttributes.lastClosingCurlyBraceToken(), attributePatterns.toString());
        return variables;
    }

    private Collection<Attribute<?, ?>> attributes() {
        final List<Attribute<?, ?>> atts = new ArrayList<>(resultType.getAttributes());
        resultType.getSubtypes().stream()
                  .flatMap(subtype -> subtype.getAttributes().stream())
                  .forEach(atts::add);
        return atts;
    }

    private boolean hasTypes() {
        if (resultType.getTypes() != null) {
            return true;
        }
        return resultType.getSubtypes().stream()
                         .anyMatch(subtype -> subtype.getTypes() != null);
    }

    private Optional<String> context(FieldSpecification<?, ?> att) {
        assert descriptor.getAttributeContexts(att).size() == 1;
        return descriptor.getSingleAttributeContext(att).map(URI::toString);
    }
}
