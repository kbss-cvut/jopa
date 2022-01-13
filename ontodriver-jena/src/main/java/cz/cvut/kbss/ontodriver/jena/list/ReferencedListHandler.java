/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;

class ReferencedListHandler extends ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    ReferencedListHandler(StorageConnector connector) {
        super(connector);
    }

    @Override
    AbstractListIterator iterator(ReferencedListDescriptor descriptor) {
        return new ReferencedListIterator(descriptor, connector);
    }

    @Override
    AbstractListIterator iterator(ReferencedListValueDescriptor descriptor) {
        return iterator((ReferencedListDescriptor) descriptor);
    }

    @Override
    void appendNewNodes(ReferencedListValueDescriptor descriptor, int i, Resource lastNode) {
        assert lastNode != null;
        final List<Statement> toAdd = new ArrayList<>((descriptor.getValues().size() - i) * 2);
        final Property hasList = createProperty(descriptor.getListProperty().getIdentifier().toString());
        final Property hasNext = createProperty(descriptor.getNextNode().getIdentifier().toString());
        final Property hasContent = createProperty(descriptor.getNodeContent().getIdentifier().toString());
        final String context = descriptor.getContext() != null ? descriptor.getContext().toString() : null;
        for (; i < descriptor.getValues().size(); i++) {
            lastNode =
                    appendNode(lastNode, descriptor.getValues().get(i), i == 0 ? hasList : hasNext, hasContent, context,
                            toAdd);
        }
        connector.add(toAdd, context);
    }

    private Resource appendNode(Resource previousNode, NamedResource value, Property link, Property hasContent,
                                String context, List<Statement> statements) {
        final Resource node = generateNewListNode(value.getIdentifier(), context);
        statements.add(createStatement(previousNode, link, node));
        statements.add(createStatement(node, hasContent, createResource(value.getIdentifier().toString())));
        return node;
    }

    private Resource generateNewListNode(URI baseUri, String context) {
        Resource node;
        int index = 0;
        Collection<Statement> statements;
        do {
            node = createResource(baseUri.toString() + "-SEQ_" + index++);
            statements = connector
                    .find(node, null, null, context != null ? Collections.singleton(context) : Collections.emptySet());
        } while (!statements.isEmpty());
        return node;
    }
}
