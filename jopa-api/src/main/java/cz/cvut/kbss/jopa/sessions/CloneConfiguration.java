package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public class CloneConfiguration {

    private final Descriptor descriptor;

    private final List<Consumer<Object>> postRegister = new ArrayList<>(1);

    public CloneConfiguration(Descriptor descriptor) {
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    public CloneConfiguration(Descriptor descriptor, List<Consumer<Object>> handlers) {
        this.descriptor = Objects.requireNonNull(descriptor);
        Objects.requireNonNull(handlers);
        postRegister.addAll(handlers);
    }

    public void addPostRegisterHandler(Consumer<Object> handler) {
        postRegister.add(handler);
    }

    public Descriptor getDescriptor() {
        return descriptor;
    }

    public List<Consumer<Object>> getPostRegister() {
        return Collections.unmodifiableList(postRegister);
    }
}
