package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.jopa.model.AttributeNode;
import cz.cvut.kbss.jopa.model.AttributeNodeImpl;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

class FetchGraphProcessor {

    private final MetamodelImpl metamodel;

    FetchGraphProcessor(MetamodelImpl metamodel) {
        this.metamodel = metamodel;
    }

    <T> EntityGraph<T> generateDefaultFetchGraph(IdentifiableEntityType<T> et) {
        final EntityGraph<T> graph = new EntityGraphImpl<>(et, metamodel);
        graph.addAttributeNodes(attributes(et).toArray(Attribute[]::new));
        return graph;
    }

    static <T> Collection<Attribute<?, ?>> attributes(IdentifiableEntityType<T> et) {
        final List<Attribute<?, ?>> atts = new ArrayList<>(et.getAttributes().stream()
                                                             .filter(att -> att.getFetchType() == FetchType.EAGER)
                                                             .toList());
        et.getSubtypes().stream()
          .flatMap(subtype -> subtype.getAttributes().stream())
          .filter(att -> att.getFetchType() == FetchType.EAGER)
          .forEach(atts::add);
        return atts;
    }

    <T> List<QueryProjectionToAxiomMapping> mapFetchGraphToProjection(EntityGraph<T> fetchGraph,
                                                                      IdentifiableEntityType<T> et,
                                                                      String rootVariable) {
        return mapFetchGraphToProjection(fetchGraph.getAttributeNodes(), et, rootVariable);
    }

    private <T> List<QueryProjectionToAxiomMapping> mapFetchGraphToProjection(List<AttributeNode<?>> attributeNodes,
                                                                              IdentifiableEntityType<T> et,
                                                                              String subjectVariable) {
        final List<QueryProjectionToAxiomMapping> mappings = new ArrayList<>(attributeNodes.size());
        final List<QueryProjectionToAxiomMapping> mappingsToAppend = new ArrayList<>();
        attributeNodes.forEach(attNode -> {
            final Attribute<?, ?> att = attribute(attNode, et);
            final String attVarName = EntityMappingQueryModifier.varName(subjectVariable, attNode.getAttributeName());
            mappings.add(new QueryProjectionToAxiomMapping(subjectVariable, attVarName, att));
            if (!attNode.getSubgraphs().isEmpty()) {
                attNode.getSubgraphs().forEach((cls, graph) -> {
                    final IdentifiableEntityType<?> subType = metamodel.entity(cls);
                    mappingsToAppend.addAll(mapFetchGraphToProjection(graph.getAttributeNodes(), subType, attVarName));
                });
            }
        });
        mappings.add(new QueryProjectionToAxiomMapping(subjectVariable,
                EntityMappingQueryModifier.varName(subjectVariable, AttributeEnumeratingSparqlAssemblyModifier.TYPES_VAR_NAME), et.getTypes()));
        mappings.addAll(mappingsToAppend);
        return mappings;
    }

    private Attribute<?, ?> attribute(AttributeNode<?> node, IdentifiableEntityType<?> et) {
        if (node instanceof AttributeNodeImpl<?> impl) {
            return impl.getAttribute();
        }
        return et.getAttributeIncludingSubTypes(node.getAttributeName());
    }

    record QueryProjectionToAxiomMapping(String subjectVariable, String objectVariable,
                                         FieldSpecification<?, ?> field) {}
}
