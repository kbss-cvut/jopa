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

    private FetchGraphWrapper(List<AttributeNode<?>> attributeNodes) {
        this.attributeNodes = attributeNodes;
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

    /**
     * Gets a subgraph for the specified attribute (if present).
     * <p>
     * Empty {@link FetchGraphWrapper} is returned if the attribute is not present or this wrapper is empty.
     *
     * @param att Attribute to get subgraph for
     * @return Fetch graph wrapper
     */
    public FetchGraphWrapper getAttributeSubgraph(FieldSpecification<?, ?> att) {
        if (!isPresent()) {
            return new FetchGraphWrapper();
        }
        return attributeNodes.stream()
                             .filter(an -> att.getName().equals(an.getAttributeName()))
                             .findFirst().map(an -> new FetchGraphWrapper(an.getSubgraphs().values().stream()
                                                                            .flatMap(sg -> sg.getAttributeNodes()
                                                                                             .stream()).toList()))
                             .orElse(new FetchGraphWrapper());
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
