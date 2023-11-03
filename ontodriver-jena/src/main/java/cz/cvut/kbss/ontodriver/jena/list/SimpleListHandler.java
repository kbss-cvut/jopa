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
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

    SimpleListHandler(StorageConnector connector) {
        super(connector);
    }

    @Override
    SimpleListIterator iterator(SimpleListDescriptor descriptor) {
        return new SimpleListIterator(descriptor, connector);
    }

    @Override
    AbstractListIterator iterator(SimpleListValueDescriptor descriptor) {
        return iterator((SimpleListDescriptor) descriptor);
    }

    @Override
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
}
