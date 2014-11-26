package cz.cvut.kbss.ontodriver_new.descriptors;

import java.net.URI;

import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * Describes a simple sequence. </p>
 * <p/>
 * Simple lists are classic Lips-style lists (singly-linked lists), where each
 * node is a subject for axiom referencing the next node.
 *
 * @author ledvima1
 */
public class SimpleListDescriptorImpl implements SimpleListDescriptor {

    protected final ListDescriptor descriptor;

    public SimpleListDescriptorImpl(NamedResource listOwner, Assertion listProperty,
                                    Assertion nextNodeProperty) {
        this.descriptor = new BaseListDescriptorImpl(listOwner, listProperty, nextNodeProperty);
    }

    @Override
    public void setContext(URI context) {
        descriptor.setContext(context);
    }

    @Override
    public URI getContext() {
        return descriptor.getContext();
    }

    @Override
    public NamedResource getListOwner() {
        return descriptor.getListOwner();
    }

    @Override
    public Assertion getListProperty() {
        return descriptor.getListProperty();
    }

    @Override
    public Assertion getNextNode() {
        return descriptor.getNextNode();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + descriptor.hashCode();
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
        SimpleListDescriptorImpl other = (SimpleListDescriptorImpl) obj;
        if (!descriptor.equals(other.descriptor))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "[SimpleList: " + descriptor + "]";
    }
}
