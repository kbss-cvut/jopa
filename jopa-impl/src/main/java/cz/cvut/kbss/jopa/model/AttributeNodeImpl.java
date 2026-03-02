package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

import java.util.HashMap;
import java.util.Map;

public class AttributeNodeImpl<X> implements AttributeNode<X> {

    private final Attribute<?, X> attribute;

    private final Map<Class, Subgraph> subgraphs = new HashMap<>();

    public AttributeNodeImpl(Attribute<?, X> attribute) {
        this.attribute = attribute;
    }

    @Override
    public String getAttributeName() {
        return attribute.getName();
    }

    @Override
    public Map<Class, Subgraph> getSubgraphs() {
        return subgraphs;
    }

    void addSubgraph(EntityGraphImpl<?> graph) {
        subgraphs.put(graph.getClassType(), graph);
    }
}
