package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Containers;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Axiom;

import java.util.Collection;

public class JenaContainers implements Containers {

    private final JenaAdapter adapter;
    private final Procedure beforeCallback;
    private final Procedure afterCallback;

    public JenaContainers(JenaAdapter adapter, Procedure beforeCallback, Procedure afterCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterCallback = afterCallback;
    }

    @Override
    public Collection<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws OntoDriverException {
        beforeCallback.execute();
        return adapter.containerHandler().readContainer(descriptor);
    }

    @Override
    public <T> void persistContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException {
        beforeCallback.execute();
        adapter.containerHandler().persistContainer(descriptor);
        afterCallback.execute();
    }

    @Override
    public <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws OntoDriverException {
        beforeCallback.execute();
        adapter.containerHandler().updateContainer(descriptor);
        afterCallback.execute();
    }
}
