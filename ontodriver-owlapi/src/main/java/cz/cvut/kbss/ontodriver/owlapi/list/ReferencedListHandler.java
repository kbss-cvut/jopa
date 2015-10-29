package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.semanticweb.owlapi.model.*;

import java.util.ArrayList;
import java.util.List;

class ReferencedListHandler extends ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final int NEXT_NODE_GENERATION_THRESHOLD = 100;

    private final OWLOntology ontology;
    private final OWLOntologyManager ontologyManager;

    ReferencedListHandler(OwlapiAdapter owlapiAdapter, OntologyStructures snapshot) {
        super(owlapiAdapter, snapshot);
        this.ontology = snapshot.getOntology();
        this.ontologyManager = snapshot.getOntologyManager();
    }

    @Override
    ReferencedListIterator iterator(ReferencedListDescriptor descriptor) {
        if (descriptor.getListProperty().isInferred() || descriptor.getNextNode().isInferred() ||
                descriptor.getNodeContent().isInferred()) {
            return new InferredReferencedListIterator(descriptor, snapshot, axiomAdapter);
        } else {
            return new ReferencedListIterator(descriptor, snapshot, axiomAdapter);
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
    public void updateList(ReferencedListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            removeObsoleteNodes(iterator(descriptor));
        } else if (isOrigEmpty(descriptor)) {
            persistList(descriptor);
        } else {
            mergeLists(descriptor);
        }
    }

    private boolean isOrigEmpty(ReferencedListValueDescriptor descriptor) {
        final OwlapiListIterator it = iterator(descriptor);
        return !it.hasNext();
    }

    private void removeObsoleteNodes(OwlapiListIterator iterator) {
        if (!iterator.hasNext()) {
            return;
        }
        final List<OWLOntologyChange> changes = new ArrayList<>();
        while (iterator.hasNext()) {
            iterator.next();
            changes.addAll(iterator.removeWithoutReconnect());
        }
        owlapiAdapter.addTransactionalChanges(ontologyManager.applyChanges(changes));
    }

    private void mergeLists(ReferencedListValueDescriptor descriptor) {
        final ReferencedListIterator it = iterator(descriptor);
        final List<NamedResource> values = descriptor.getValues();
        final List<OWLOntologyChange> changes = new ArrayList<>(values.size());
        int i = 0;
        NamedResource lastNode = null;
        while (it.hasNext() && i < values.size()) {
            final NamedResource newValue = values.get(i);
            final NamedResource currentValue = it.nextValue();
            if (!newValue.equals(currentValue)) {
                changes.addAll(ontologyManager.applyChanges(it.replaceNode(newValue)));
            }
            lastNode = it.getCurrentNode();
            i++;
        }
        owlapiAdapter.addTransactionalChanges(changes);
        assert lastNode != null;
        removeObsoleteNodes(it);
        addNewNodes(descriptor, i, lastNode);
    }

    private void addNewNodes(ReferencedListValueDescriptor descriptor, int index, NamedResource lastNode) {
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
        owlapiAdapter.addTransactionalChanges(ontologyManager.applyChanges(nodeGenerator.getChanges()));
    }

    private class ReferencedListNodeGenerator {

        private final String baseUri;
        private final List<OWLOntologyChange> changes;
        private final Assertion nodeContentProperty;

        private int index;
        private Assertion property;

        public ReferencedListNodeGenerator(NamedResource baseUri, Assertion nodeContent) {
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
