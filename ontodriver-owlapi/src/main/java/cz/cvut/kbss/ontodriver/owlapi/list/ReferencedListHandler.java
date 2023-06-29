/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.ArrayList;
import java.util.List;

class ReferencedListHandler extends ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final int NEXT_NODE_GENERATION_THRESHOLD = 100;

    ReferencedListHandler(OwlapiAdapter owlapiAdapter, OntologySnapshot snapshot) {
        super(owlapiAdapter, snapshot);
    }

    @Override
    OwlapiListIterator iterator(ListDescriptor descriptor) {
        assert descriptor instanceof ReferencedListDescriptor;

        final ReferencedListDescriptor desc = (ReferencedListDescriptor) descriptor;
        if (desc.getListProperty().isInferred() || desc.getNextNode().isInferred() ||
                desc.getNodeContent().isInferred()) {
            return new InferredReferencedListIterator(desc, snapshot, axiomAdapter);
        } else {
            return new ReferencedListIterator(desc, snapshot, axiomAdapter);
        }
    }

    @Override
    List<OWLOntologyChange> createListAxioms(ReferencedListValueDescriptor descriptor) {
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(
                descriptor.getListOwner(), descriptor.getNodeContent());
        boolean first = true;
        NamedResource previousNode = descriptor.getListOwner();
        nodeGenerator.setIndex(0);
        for (NamedResource value : descriptor.getValues()) {
            if (first) {
                nodeGenerator.setProperty(descriptor.getListProperty());
                previousNode = nodeGenerator.addListNode(previousNode, value);
                first = false;
            } else {
                nodeGenerator.setProperty(descriptor.getNextNode());
                previousNode = nodeGenerator.addListNode(previousNode, value);
            }
        }
        return nodeGenerator.getChanges();
    }

    @Override
    boolean isOrigEmpty(ReferencedListValueDescriptor descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
        return !it.hasNext();
    }

    @Override
    void addNewNodes(ReferencedListValueDescriptor descriptor, int index, NamedResource lastNode) {
        if (index >= descriptor.getValues().size()) {
            return;
        }
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(descriptor.getListOwner(),
                descriptor.getNodeContent());
        nodeGenerator.setProperty(descriptor.getNextNode()); // We know that we are past list head
        nodeGenerator.setIndex(index);
        for (; index < descriptor.getValues().size(); index++) {
            final NamedResource nextValue = descriptor.getValues().get(index);
            lastNode = nodeGenerator.addListNode(lastNode, nextValue);
        }
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(nodeGenerator.getChanges()));
    }

    private class ReferencedListNodeGenerator {

        private final String baseUri;
        private final List<OWLOntologyChange> changes;
        private final Assertion nodeContentProperty;

        private int index;
        private Assertion property;

        ReferencedListNodeGenerator(NamedResource baseUri, Assertion nodeContent) {
            this.baseUri = baseUri.toString() + "-SEQ_";
            this.changes = new ArrayList<>();
            this.nodeContentProperty = nodeContent;
        }

        private void setIndex(int index) {
            this.index = index;
        }

        private void setProperty(Assertion property) {
            this.property = property;
        }

        private List<OWLOntologyChange> getChanges() {
            return changes;
        }

        private NamedResource addListNode(NamedResource previousNode, NamedResource value) {
            assert property != null;

            final NamedResource node = generateNode();
            final OWLAxiom nodeAxiom = axiomAdapter
                    .toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(previousNode, property, new Value<>(node)));
            changes.add(new MutableAddAxiom(ontology, nodeAxiom));
            changes.add(generateNodeContent(node, value));
            index++;
            return node;
        }

        private OWLOntologyChange generateNodeContent(NamedResource node, NamedResource value) {
            final OWLAxiom valueAxiom = axiomAdapter
                    .toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(node, nodeContentProperty, new Value<>(value)));
            return new MutableAddAxiom(ontology, valueAxiom);
        }

        private NamedResource generateNode() {
            int i = index;
            IRI iri;
            do {
                iri = IRI.create(baseUri + i);
                if (!ontology.containsIndividualInSignature(iri)) {
                    return NamedResource.create(iri.toURI());
                }
                i++;
            }
            while (i < (i + NEXT_NODE_GENERATION_THRESHOLD));
            throw new IdentifierGenerationException(
                    "Unable to generate identifier for sequence node with base " + baseUri);
        }
    }
}
