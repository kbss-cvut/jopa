package cz.cvut.kbss.ontodriver.descriptor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

public class ReferencedListValueDescriptor extends ReferencedListDescriptorImpl implements
        ListValueDescriptor {

    private final List<NamedResource> values;

    public ReferencedListValueDescriptor(NamedResource listOwner, Assertion listProperty,
                                         Assertion nextNode, Assertion nodeContent) {
        super(listOwner, listProperty, nextNode, nodeContent);
        this.values = new ArrayList<>();
    }

    @Override
    public List<NamedResource> getValues() {
        return Collections.unmodifiableList(values);
    }

    public void addValue(NamedResource value) {
        Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));
        values.add(value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + values.hashCode();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (getClass() != obj.getClass())
            return false;
        ReferencedListValueDescriptor other = (ReferencedListValueDescriptor) obj;
        if (!descriptor.equals(other.descriptor))
            return false;
        if (!getNodeContent().equals(other.getNodeContent()))
            return false;
        if (!values.equals(other.values))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "[ReferencedListValueDescriptor: owner = " + descriptor.getListOwner() + ", values = " + values + "]";
    }
}
