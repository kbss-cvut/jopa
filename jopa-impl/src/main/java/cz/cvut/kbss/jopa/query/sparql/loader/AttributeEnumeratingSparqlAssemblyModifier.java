package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.sparql.QueryAttributes;
import cz.cvut.kbss.jopa.query.sparql.TokenQueryParameter;
import cz.cvut.kbss.jopa.query.sparql.TokenStreamSparqlQueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import org.antlr.v4.runtime.TokenStreamRewriter;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Optimizes entity loading by modifying the query to fetch all named attributes.
 * <p>
 * This optimizer is applicable for SELECT queries that select instances of an entity class. Instead of loading the
 * instances one by one after the query is evaluated, this optimizer modifies the query to fetch all available entity
 * attributes by injecting optional triple patterns for each of the entity attributes and projecting the values from the
 * query. If the result type has subclasses, all attributes of the subclasses are enumerated in the query so that the
 * instance loading can then determine the result type and load the appropriate instance with all the relevant attribute
 * data.
 * <p>
 * The injected patterns look like this:
 * <pre>
 * OPTIONAL { ?subject &lt;property&gt; ?value }
 * </pre>
 * If a descriptor with at most one context (for each attribute) is provided, the injected patterns look like this:
 * <pre>
 * OPTIONAL { GRAPH ?g { ?subject &lt;property&gt; ?value } }
 * </pre>
 * Where {@literal ?g} is determined from the descriptor.
 * <p>
 * When <b>not</b> to use this modifier:
 * <ul>
 *     <li>When the result type has {@literal Properties} field</li>
 * </ul>
 */
public class AttributeEnumeratingSparqlAssemblyModifier implements SparqlAssemblyModifier {

    static final String TYPES_VAR_SUFFIX = "types";

    private final IdentifiableEntityType<?> resultType;

    private final Descriptor descriptor;

    private final boolean inferredAttsInDefault;

    public AttributeEnumeratingSparqlAssemblyModifier(IdentifiableEntityType<?> resultType, Descriptor descriptor,
                                                      ConnectionWrapper connection) {
        this.resultType = resultType;
        this.descriptor = descriptor;
        this.inferredAttsInDefault = resolveInferenceContext(connection);
        assert resultType.getProperties() == null;
    }

    private boolean resolveInferenceContext(ConnectionWrapper connection) {
        return "GraphDB".equals(connection.getRepositoryMetadata().getProductName());
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
            final int min = Arrays.stream(att.getConstraints()).map(ParticipationConstraint::min)
                                  .min(Comparator.naturalOrder()).orElse(0);
            final String variable = "?" + subjectParamName + att.getName();
            variables.add(variable);
            if (min < 1) {
                attributePatterns.append("OPTIONAL { ");
            }
            final Optional<String> ctx = context(att);
            ctx.ifPresent(uri -> attributePatterns.append("GRAPH <").append(uri).append("> { "));
            attributePatterns.append(subjectVariable).append(" <").append(att.getIRI())
                             .append("> ").append(variable).append(" . ");
            if (min < 1) {
                attributePatterns.append("} ");
            }
            ctx.ifPresent(uri -> attributePatterns.append("} "));
        });
        final String variable = "?" + subjectParamName + TYPES_VAR_SUFFIX;
        variables.add(variable);
        final Optional<String> ctx = typesContext();
        ctx.ifPresent(uri -> attributePatterns.append("GRAPH <").append(uri).append("> { "));
        attributePatterns.append(subjectVariable).append(" a ").append(variable).append(" . ");
        ctx.ifPresent(uri -> attributePatterns.append("} "));
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

    private Optional<String> context(FieldSpecification<?, ?> att) {
        assert descriptor.getAttributeContexts(att).size() <= 1;
        if (att.isInferred() && inferredAttsInDefault) {
            return Optional.empty();
        }
        return descriptor.getSingleAttributeContext(att).map(URI::toString);
    }

    private Optional<String> typesContext() {
        final Optional<? extends TypesSpecification<?, ?>> optionalTs = resultType.getTypes() != null ? Optional.of(resultType.getTypes()) : resultType.getSubtypes()
                                                                                                                                                       .stream()
                                                                                                                                                       .map(AbstractIdentifiableType::getTypes)
                                                                                                                                                       .filter(Objects::nonNull)
                                                                                                                                                       .findFirst();
        return optionalTs.flatMap(ts -> {
            assert descriptor.getAttributeContexts(ts).size() <= 1;
            return descriptor.getSingleAttributeContext(ts);
        }).map(URI::toString);
    }
}
