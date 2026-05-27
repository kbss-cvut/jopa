package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.annotations.NamedAttributeNode;
import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph;
import cz.cvut.kbss.jopa.model.annotations.NamedSubgraph;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.MetamodelClassMapper;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Builds {@link EntityGraphImpl} instances from {@link NamedEntityGraph} annotations discovered on entity classes.
 */
public class NamedEntityGraphProcessor {

    private final NamedEntityGraphManager manager;

    private final MetamodelClassMapper metamodel;

    public NamedEntityGraphProcessor(MetamodelClassMapper metamodel) {
        this.metamodel = metamodel;
        this.manager = new NamedEntityGraphManager();
    }

    public NamedEntityGraphManager getManager() {
        return manager;
    }

    /**
     * Builds an entity graph from the specified {@link NamedEntityGraph} declaration and registers it in the manager.
     *
     * @param rootType    The class on which the {@code NamedEntityGraph} annotation was declared
     * @param declaration Named entity graph annotation
     */
    public void buildEntityGraph(Class<?> rootType, NamedEntityGraph declaration) {
        Objects.requireNonNull(rootType);
        Objects.requireNonNull(declaration);
        final String name = graphName(rootType, declaration);
        manager.addEntityGraph(name, createGraph(rootType, name, declaration));
    }

    private static String graphName(Class<?> rootType, NamedEntityGraph declaration) {
        if (declaration.name().isEmpty()) {
            return rootType.getSimpleName();
        }
        return declaration.name();
    }

    private <T> EntityGraphImpl<T> createGraph(Class<?> rootType, String name, NamedEntityGraph declaration) {
        final AbstractIdentifiableType<T> entityType = metamodel.entity((Class<T>) rootType);
        final EntityGraphImpl<T> graph = new EntityGraphImpl<>(name, entityType, metamodel);
        final Map<String, NamedSubgraph> namedSubgraphs = indexSubgraphs(declaration.subgraphs());
        addAttributeNodes(declaration.attributeNodes(), graph, entityType, namedSubgraphs);
        return graph;
    }

    private <T> void addAttributeNodes(NamedAttributeNode[] nodes, EntityGraphImpl<T> graph,
                                       AbstractIdentifiableType<T> entityType,
                                       Map<String, NamedSubgraph> namedSubgraphs) {
        for (NamedAttributeNode node : nodes) {
            graph.addAttributeNodes(node.value());
            if (!node.subgraph().isEmpty()) {
                attachSubgraph(graph, entityType, node, namedSubgraphs);
            }
        }
    }

    private void attachSubgraph(EntityGraphImpl<?> parent, AbstractIdentifiableType<?> rootEntityType,
                                NamedAttributeNode node, Map<String, NamedSubgraph> namedSubgraphs) {
        final NamedSubgraph subDecl = namedSubgraphs.get(node.subgraph());
        if (subDecl == null) {
            throw new IllegalArgumentException(
                    "Named subgraph '" + node.subgraph() + "' referenced by attribute '" + node.value() +
                            "' is not declared in the entity graph.");
        }
        final Class<?> subType = resolveSubgraphType(rootEntityType, node.value(), subDecl);
        final EntityGraphImpl<?> sub = buildSubgraph(subType, subDecl, namedSubgraphs);
        final AttributeNodeImpl<?> attrNode = parent.getAttributeNode(node.value());
        attrNode.addSubgraph(sub);
    }

    private <S> EntityGraphImpl<S> buildSubgraph(Class<?> subType, NamedSubgraph declaration,
                                                 Map<String, NamedSubgraph> namedSubgraphs) {
        final AbstractIdentifiableType<S> entityType = metamodel.entity((Class<S>) subType);
        final EntityGraphImpl<S> graph = new EntityGraphImpl<>(declaration.name(), entityType, metamodel);
        addAttributeNodes(declaration.attributeNodes(), graph, entityType, namedSubgraphs);
        return graph;
    }

    private static Class<?> resolveSubgraphType(AbstractIdentifiableType<?> rootEntityType, String attributeName,
                                                NamedSubgraph subDecl) {
        if (subDecl.type() != void.class) {
            return subDecl.type();
        }
        final Attribute<?, ?> att = rootEntityType.getAttribute(attributeName);
        assert att != null;
        if (att.isCollection()) {
            return ((PluralAttribute<?, ?, ?>) att).getBindableJavaType();
        }
        return att.getJavaType();
    }

    private static Map<String, NamedSubgraph> indexSubgraphs(NamedSubgraph[] subgraphs) {
        if (subgraphs.length == 0) {
            return Map.of();
        }
        final Map<String, NamedSubgraph> result = new HashMap<>(subgraphs.length);
        for (NamedSubgraph s : subgraphs) {
            result.put(s.name(), s);
        }
        return result;
    }
}
