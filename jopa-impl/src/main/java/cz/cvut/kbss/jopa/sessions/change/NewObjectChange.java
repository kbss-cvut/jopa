package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.Objects;

/**
 * Represents addition of a new object.
 */
public class NewObjectChange implements Change {

    private final Object object;

    private final Descriptor descriptor;

    public NewObjectChange(Object object, Descriptor descriptor) {
        this.object = Objects.requireNonNull(object);
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    @Override
    public Object getClone() {
        return object;
    }

    @Override
    public Object getOriginal() {
        return null;
    }

    @Override
    public Descriptor getDescriptor() {
        return descriptor;
    }
}
