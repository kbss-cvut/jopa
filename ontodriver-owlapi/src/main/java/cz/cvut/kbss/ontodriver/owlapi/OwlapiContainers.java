package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Containers;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.owlapi.util.Procedure;

import java.util.Collection;

public class OwlapiContainers implements Containers {

    private final OwlapiAdapter adapter;
    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    public OwlapiContainers(OwlapiAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Collection<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws OntoDriverException {
        beforeCallback.execute();
        return adapter.getContainerHandler().readContainer(descriptor);
    }

    @Override
    public <T> void persistContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException {
        beforeCallback.execute();
        adapter.getContainerHandler().persistContainer(descriptor);
        afterChangeCallback.execute();
    }

    @Override
    public <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException {
        beforeCallback.execute();
        adapter.getContainerHandler().updateContainer(descriptor);
        afterChangeCallback.execute();
    }
}
