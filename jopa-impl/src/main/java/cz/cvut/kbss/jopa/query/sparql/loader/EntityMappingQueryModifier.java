package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.AttributeNode;
import cz.cvut.kbss.jopa.model.AttributeNodeImpl;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
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

/**
 * Creates a basic graph pattern corresponding to the specified entity fetch graph.
 */
class EntityMappingQueryModifier {

    private final MetamodelImpl metamodel;
    private final IdentifiableEntityType<?> resultType;

    private final Descriptor descriptor;

    private final boolean inferredAttsInDefault;

    EntityMappingQueryModifier(MetamodelImpl metamodel, IdentifiableEntityType<?> resultType, Descriptor descriptor,
                               boolean inferredAttsInDefault) {
        this.metamodel = metamodel;
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
        final StringBuilder optionalAttributesPatterns = new StringBuilder();
        final List<String> variables = new ArrayList<>();
        final List<String> variablesToAppend = new ArrayList<>();
        final String subjectVariable = "?" + subjectVar;
        attributeNodes.forEach(attNode -> {
            final Attribute<?, ?> att = attribute(attNode);
            final int min = Arrays.stream(att.getConstraints()).map(ParticipationConstraint::min)
                                  .min(Comparator.naturalOrder()).orElse(0);
            final String variable = "?" + varName(subjectVar, att.getName());
            variables.add(variable);
            final StringBuilder target = min < 1 ? optionalAttributesPatterns : attributePatterns;
            if (min < 1) {
                target.append("OPTIONAL { ");
            }
            final Optional<String> ctx = context(att);
            ctx.ifPresent(uri -> target.append("GRAPH <").append(uri).append("> { "));
            target.append(subjectVariable).append(" <").append(att.getIRI())
                  .append("> ").append(variable).append(" . ");
            if (!attNode.getSubgraphs().isEmpty()) {
                attNode.getSubgraphs().forEach((cls, graph) -> {
                    final IdentifiableEntityType<?> subType = metamodel.entity(cls);
                    final EntityMappingQueryModifier subModifier = new EntityMappingQueryModifier(metamodel, subType, descriptor.getAttributeDescriptor(att), inferredAttsInDefault);
                    final QueryModification mod = subModifier.modify(graph, varName(subjectVar, att.getName()));
                    target.append(mod.queryPart());
                    variablesToAppend.addAll(mod.variables());
                });
            }
            if (min < 1) {
                target.append("} ");
            }
            ctx.ifPresent(uri -> target.append("} "));
        });
        final String typesVariable = "?" + subjectVar + TYPES_VAR_SUFFIX;
        variables.add(typesVariable);
        final Optional<String> ctx = typesContext();
        ctx.ifPresent(uri -> attributePatterns.append("GRAPH <").append(uri).append("> { "));
        attributePatterns.append(subjectVariable).append(" a ").append(typesVariable).append(" . ");
        ctx.ifPresent(uri -> attributePatterns.append("} "));
        variables.addAll(variablesToAppend);
        // PERF: Put OPTIONAL patterns to the end
        return new QueryModification(variables, attributePatterns + optionalAttributesPatterns.toString());
    }

    private static String varName(String subjectVar, String attName) {
        return subjectVar + "_" + attName;
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
