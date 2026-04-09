package cz.cvut.kbss.jopa.sessions.util;

import cz.cvut.kbss.jopa.model.AttributeNode;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.List;
import java.util.Objects;

/**
 * Wrapper for {@link EntityGraph} or {@link Subgraph} providing a unified API.
 */
public class FetchGraphWrapper {

    private final List<AttributeNode<?>> attributeNodes;

    public FetchGraphWrapper() {
        this.attributeNodes = null;
    }

    public FetchGraphWrapper(EntityGraph<?> entityGraph) {
        this.attributeNodes = entityGraph != null ? entityGraph.getAttributeNodes() : null;
    }

    public FetchGraphWrapper(Subgraph<?> subgraph) {
        assert subgraph != null;
        this.attributeNodes = subgraph.getAttributeNodes();
    }

    public List<AttributeNode<?>> getAttributeNodes() {
        return attributeNodes;
    }

    public boolean isPresent() {
        return attributeNodes != null;
    }

    public boolean hasAttribute(FieldSpecification<?, ?> att) {
        return isPresent() && attributeNodes.stream().anyMatch(an -> att.getName().equals(an.getAttributeName()));
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof FetchGraphWrapper that)) {
            return false;
        }
        return Objects.equals(attributeNodes, that.attributeNodes);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(attributeNodes);
    }
}
