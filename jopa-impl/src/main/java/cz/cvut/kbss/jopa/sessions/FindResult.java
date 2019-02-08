package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.sessions.descriptor.InstanceDescriptor;

import java.util.Optional;

/**
 * Represents the result of instance loading from the repository.
 *
 * @param <T> Type of the result
 */
public class FindResult<T> {

    /**
     * Empty find result
     */
    public static final FindResult EMPTY = new FindResult<>(null, null);

    private final T instance;
    private final InstanceDescriptor<T> instanceDescriptor;


    public FindResult(T instance, InstanceDescriptor<T> instanceDescriptor) {
        this.instance = instance;
        this.instanceDescriptor = instanceDescriptor;
    }

    public Optional<T> getInstance() {
        return Optional.ofNullable(instance);
    }

    public InstanceDescriptor<T> getInstanceDescriptor() {
        return instanceDescriptor;
    }

    @SuppressWarnings("unchecked")
    public static <T> FindResult<T> empty() {
        return EMPTY;
    }
}
