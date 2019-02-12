package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;

import java.net.URI;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;

/**
 * Change set representing object deletion.
 * <p>
 * Supports only a subset of the {@link ObjectChangeSet} operations relevant for object removal during merge into parent
 * session.
 */
public class DeleteObjectChangeSet implements ObjectChangeSet {

    private final Object object;

    private final Descriptor descriptor;

    public DeleteObjectChangeSet(Object object, Descriptor descriptor) {
        this.object = Objects.requireNonNull(object);
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    /**
     * @throws UnsupportedOperationException This method is not supported
     */
    @Override
    public void addChangeRecord(ChangeRecord record) {
        unsupported();
    }

    private static void unsupported() {
        throw new UnsupportedOperationException("Can't invoke on " + DeleteObjectChangeSet.class.getSimpleName());
    }

    @Override
    public Class<?> getObjectClass() {
        return object.getClass();
    }

    @Override
    public Set<ChangeRecord> getChanges() {
        return Collections.emptySet();
    }

    @Override
    public boolean hasChanges() {
        return false;
    }

    /**
     * @throws UnsupportedOperationException This method is not supported
     */
    @Override
    public void setNew(boolean isNew) {
        unsupported();
    }

    @Override
    public boolean isNew() {
        return false;
    }

    @Override
    public Object getCloneObject() {
        return object;
    }

    @Override
    public Object getChangedObject() {
        return object;
    }

    @Override
    public Descriptor getEntityDescriptor() {
        return descriptor;
    }

    @Override
    public URI getEntityContext() {
        return descriptor.getContext();
    }
}
