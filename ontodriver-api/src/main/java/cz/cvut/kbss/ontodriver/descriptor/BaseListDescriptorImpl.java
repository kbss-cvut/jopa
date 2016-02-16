package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.util.ErrorUtils;

import java.net.URI;
import java.util.Objects;

final class BaseListDescriptorImpl implements ListDescriptor {

    private final NamedResource listOwner;
    private final Assertion listProperty;
    private final Assertion nextNode;

    private URI context;

    public BaseListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
                                  Assertion nextNode) {
        this.listOwner = Objects.requireNonNull(listOwner, ErrorUtils.npxMessage("listOwner"));
        this.listProperty = Objects.requireNonNull(listProperty, ErrorUtils.npxMessage("listProperty"));
        this.nextNode = Objects.requireNonNull(nextNode, ErrorUtils.npxMessage("nextNode"));
    }

    @Override
    public URI getContext() {
        return context;
    }

    @Override
    public void setContext(URI context) {
        // null permitted here
        this.context = context;
    }

    @Override
    public NamedResource getListOwner() {
        return listOwner;
    }

    @Override
    public Assertion getListProperty() {
        return listProperty;
    }

    @Override
    public Assertion getNextNode() {
        return nextNode;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + listOwner.hashCode();
        result = prime * result + listProperty.hashCode();
        result = prime * result + nextNode.hashCode();
        result = prime * result + ((context == null) ? 0 : context.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        BaseListDescriptorImpl other = (BaseListDescriptorImpl) obj;
        if (!listOwner.equals(other.listOwner))
            return false;
        if (!listProperty.equals(other.listProperty))
            return false;
        if (!nextNode.equals(other.nextNode))
            return false;
        if (context == null) {
            if (other.context != null)
                return false;
        } else if (!context.equals(other.context))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "owner = " + listOwner + ", list = " + listProperty + ", next node = " + nextNode + ", context = " +
                context;
    }
}
