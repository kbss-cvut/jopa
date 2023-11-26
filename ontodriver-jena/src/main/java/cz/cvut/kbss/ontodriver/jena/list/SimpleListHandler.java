/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

public class SimpleListHandler {

    private final StorageConnector connector;

    public SimpleListHandler(StorageConnector connector) {
        this.connector = connector;
    }

    List<Axiom<NamedResource>> loadList(SimpleListDescriptor descriptor) {
        final List<Axiom<NamedResource>> result = new ArrayList<>();
        final AbstractListIterator it = new SimpleListIterator(descriptor, connector);
        while (it.hasNext()) {
            result.add(it.nextAxiom());
        }
        return result;
    }

    void persistList(SimpleListValueDescriptor descriptor) {
        final List<NamedResource> values = descriptor.getValues();
        if (values.isEmpty()) {
            return;
        }
        Resource owner = createResource(descriptor.getListOwner().getIdentifier().toString());
        appendNewNodes(descriptor, 0, owner);
    }

    void appendNewNodes(SimpleListValueDescriptor descriptor, int index, Resource lastNode) {
        final Property hasList = ResourceFactory
                .createProperty(descriptor.getListProperty().getIdentifier().toString());
        final Property hasNext = ResourceFactory.createProperty(descriptor.getNextNode().getIdentifier().toString());
        final List<Statement> toAdd = new ArrayList<>(descriptor.getValues().size() - index);
        for (; index < descriptor.getValues().size(); index++) {
            lastNode = appendNode(lastNode, index == 0 ? hasList : hasNext, descriptor.getValues().get(index), toAdd);
        }
        final URI context = descriptor.getContext();
        connector.add(toAdd, context != null ? context.toString() : null);
    }

    private static Resource appendNode(Resource previous, Property property, NamedResource value,
                                       List<Statement> statements) {
        final Resource node = createResource(value.getIdentifier().toString());
        statements.add(ResourceFactory.createStatement(previous, property, node));
        return node;
    }

    void updateList(SimpleListValueDescriptor descriptor) {
        final AbstractListIterator it = new SimpleListIterator(descriptor, connector);
        int i = 0;
        while (it.hasNext() && i < descriptor.getValues().size()) {
            final NamedResource update = descriptor.getValues().get(i);
            final NamedResource existing = it.nextValue();
            if (!existing.equals(update)) {
                it.replace(createResource(update.getIdentifier().toString()));
            }
            i++;
        }
        removeObsoleteNodes(it);
        if (i < descriptor.getValues().size()) {
            appendNewNodes(descriptor, i, it.getCurrentNode());
        }
    }

    private static void removeObsoleteNodes(AbstractListIterator it) {
        while (it.hasNext()) {
            it.nextValue();
            it.removeWithoutReconnect();
        }
    }
}
