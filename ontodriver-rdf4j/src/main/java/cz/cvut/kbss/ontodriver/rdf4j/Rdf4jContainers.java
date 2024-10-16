package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Containers;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

class Rdf4jContainers implements Containers {

    private final Rdf4jAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    Rdf4jContainers(Rdf4jAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Collection<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws OntoDriverException {
        beforeCallback.execute();
        return adapter.getContainerHandler().loadContainer(descriptor);
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
