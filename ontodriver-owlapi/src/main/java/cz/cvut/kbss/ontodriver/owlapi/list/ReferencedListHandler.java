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
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.util.ArrayList;
import java.util.List;

class ReferencedListHandler extends ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final int NEXT_NODE_GENERATION_THRESHOLD = 100;

    private final OWLOntology ontology;

    ReferencedListHandler(OwlapiAdapter owlapiAdapter, OntologyStructures snapshot) {
        super(owlapiAdapter, snapshot);
        this.ontology = snapshot.getOntology();
    }

    @Override
    OwlapiListIterator iterator(ReferencedListDescriptor descriptor) {
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
                descriptor.getListOwner().toString(), descriptor.getNodeContent());
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
        // TODO
    }

    private class ReferencedListNodeGenerator {

        private final String baseUri;
        private final List<OWLOntologyChange> changes;
        private final Assertion nodeContentProperty;

        private int index;
        private Assertion property;

        public ReferencedListNodeGenerator(String baseUri, Assertion nodeContent) {
            this.baseUri = baseUri + "-SEQ_";
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
