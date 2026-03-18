package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.AttributeNode;
import cz.cvut.kbss.jopa.model.AttributeNodeImpl;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static cz.cvut.kbss.jopa.query.sparql.loader.AttributeEnumeratingSparqlAssemblyModifier.TYPES_VAR_SUFFIX;

class EntityMappingQueryModifier {

    private final IdentifiableEntityType<?> resultType;

    private final Descriptor descriptor;

    private final boolean inferredAttsInDefault;

    EntityMappingQueryModifier(IdentifiableEntityType<?> resultType, Descriptor descriptor,
                               boolean inferredAttsInDefault) {
        this.resultType = resultType;
        this.descriptor = descriptor;
        this.inferredAttsInDefault = inferredAttsInDefault;
    }

    /**
     * Creates a query modification based on the specified fetch graph.
     * <p>
     * That is only attributes specified in the graph are included in the query.
     *
     * @param graph      Graph describing what to fetch
     * @param subjectVar Name of the variable representing the subject of the graph patterns. Will be used to generate
     *                   the projected variable names
     * @return Query modification
     */
    QueryModification modify(EntityGraph<?> graph, String subjectVar) {
        return modifyImpl(graph.getAttributeNodes(), subjectVar);
    }

    /**
     * Creates a query modification based on the specified fetch subgraph.
     * <p>
     * That is only attributes specified in the subgraph are included in the query.
     *
     * @param graph      Graph describing what to fetch
     * @param subjectVar Name of the variable representing the subject of the graph patterns. Will be used to generate
     *                   the projected variable names
     * @return Query modification
     */
    QueryModification modify(Subgraph<?> graph, String subjectVar) {
        return modifyImpl(graph.getAttributeNodes(), subjectVar);
    }

    private QueryModification modifyImpl(List<AttributeNode<?>> attributeNodes, String subjectVar) {
        final StringBuilder attributePatterns = new StringBuilder();
        final List<String> variables = new ArrayList<>();
        final String subjectVariable = "?" + subjectVar;
        attributeNodes.forEach(attNode -> {
            final Attribute<?, ?> att = attribute(attNode);
            final int min = Arrays.stream(att.getConstraints()).map(ParticipationConstraint::min)
                                  .min(Comparator.naturalOrder()).orElse(0);
            final String variable = "?" + subjectVar + att.getName();
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
            // TODO handle subgraphs
        });
        final String variable = "?" + subjectVar + TYPES_VAR_SUFFIX;
        variables.add(variable);
        final Optional<String> ctx = typesContext();
        ctx.ifPresent(uri -> attributePatterns.append("GRAPH <").append(uri).append("> { "));
        attributePatterns.append(subjectVariable).append(" a ").append(variable).append(" . ");
        ctx.ifPresent(uri -> attributePatterns.append("} "));
        return new QueryModification(variables, attributePatterns.toString());
    }

    private Attribute<?, ?> attribute(AttributeNode<?> node) {
        if (node instanceof AttributeNodeImpl<?> impl) {
            return impl.getAttribute();
        }
        return resultType.getAttributeIncludingSubTypes(node.getAttributeName());
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

    /**
     * Query modification.
     *
     * @param variables Variables to project
     * @param queryPart Basic graph patterns to add to the query
     */
    record QueryModification(List<String> variables, String queryPart) {}
}
