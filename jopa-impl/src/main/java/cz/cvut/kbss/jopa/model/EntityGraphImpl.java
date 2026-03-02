package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EntityGraphImpl<T> implements EntityGraph<T>, Subgraph<T> {

    private final String name;

    private final ManagedType<T> entityType;
    private final Metamodel metamodel;

    private final Map<String, AttributeNodeImpl<?>> attributeNodes = new HashMap<>();

    public EntityGraphImpl(ManagedType<T> entityType, Metamodel metamodel) {
        this.entityType = entityType;
        this.metamodel = metamodel;
        this.name = null;
    }

    public EntityGraphImpl(String name, ManagedType<T> entityType, Metamodel metamodel) {
        this.name = name;
        this.entityType = entityType;
        this.metamodel = metamodel;
    }

    @Override
    public void addAttributeNodes(Attribute<T, ?>... attribute) {
        for (Attribute<T, ?> att : attribute) {
            attributeNodes.put(att.getName(), new AttributeNodeImpl<>(att));
        }
    }

    @Override
    public void addAttributeNodes(String... attributeName) {
        for (String attName : attributeName) {
            final Attribute<? super T, ?> att = entityType.getAttribute(attName);
            assert att != null;
            attributeNodes.put(attName, new AttributeNodeImpl<>(att));
        }
    }

    @Override
    public <X> EntityGraphImpl<X> addSubgraph(Attribute<T, X> attribute) {
        final AttributeNodeImpl<X> node = (AttributeNodeImpl<X>) attributeNodes.computeIfAbsent(attribute.getName(), k -> new AttributeNodeImpl<>(attribute));
        Class<?> javaType = attribute.getJavaType();
        if (attribute.isCollection()) {
            javaType = ((PluralAttribute<T, ?, X>) attribute).getBindableJavaType();
        }
        final EntityGraphImpl<X> g = new EntityGraphImpl(metamodel.entity(javaType), metamodel);
        node.addSubgraph(g);
        return g;
    }

    @Override
    public <X> Subgraph<X> addSubgraph(String attributeName) {
        final Attribute<T, X> att = (Attribute<T, X>) entityType.getAttribute(attributeName);
        return addSubgraph(att);
    }

    @Override
    public <X> Subgraph<? extends X> addSubgraph(Attribute<T, X> attribute, Class<? extends X> cls) {
        final EntityGraphImpl<X> g = addSubgraph(attribute);
        g.addSubclassSubgraph(cls);
        return g;
    }

    @Override
    public Subgraph<? extends T> addSubclassSubgraph(Class<? extends T> cls) {
        // TODO
        return null;
    }

    @Override
    public List<AttributeNode<?>> getAttributeNodes() {
        return new ArrayList<>(attributeNodes.values());
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Class<T> getClassType() {
        return entityType.getJavaType();
    }
}
