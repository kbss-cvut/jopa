/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
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
