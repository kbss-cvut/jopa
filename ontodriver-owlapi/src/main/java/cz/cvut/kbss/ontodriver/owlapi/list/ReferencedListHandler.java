package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.semanticweb.owlapi.model.*;

import java.util.ArrayList;
import java.util.List;

class ReferencedListHandler extends ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final int NEXT_NODE_GENERATION_THRESHOLD = 100;

    private final OWLOntology ontology;
    private final OWLOntologyManager manager;

    ReferencedListHandler(OwlapiAdapter owlapiAdapter, OntologyStructures snapshot) {
        super(owlapiAdapter, snapshot);
        this.ontology = snapshot.getOntology();
        this.manager = snapshot.getOntologyManager();
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
    public void persistList(ReferencedListValueDescriptor descriptor) {
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        owlapiAdapter.addTransactionalChanges(manager.applyChanges(createListAxioms(descriptor)));
    }

    private List<OWLOntologyChange> createListAxioms(ReferencedListValueDescriptor descriptor) {
        final List<OWLOntologyChange> changes = new ArrayList<>(descriptor.getValues().size() * 2);
        boolean first = true;
        final String nodeUriBase = descriptor.getListOwner().toString() + "-SEQ_";
        NamedResource previousNode = descriptor.getListOwner();
        int i = 0;
        for (NamedResource value : descriptor.getValues()) {
            final NamedResource node = generateNode(nodeUriBase, i);
            final OWLAxiom axiom;
            if (first) {
                axiom = axiomAdapter.toOwlObjectPropertyAssertionAxiom(
                        new AxiomImpl<>(previousNode, descriptor.getListProperty(), new Value<>(node)));
                first = false;
            } else {
                axiom = axiomAdapter.toOwlObjectPropertyAssertionAxiom(
                        new AxiomImpl<>(previousNode, descriptor.getNextNode(), new Value<>(node)));
            }
            changes.add(new MutableAddAxiom(ontology, axiom));
            final OWLAxiom valueAxiom = axiomAdapter.toOwlObjectPropertyAssertionAxiom(
                    new AxiomImpl<>(node, descriptor.getNodeContent(), new Value<>(value)));
            changes.add(new MutableAddAxiom(ontology, valueAxiom));
            previousNode = node;
            i++;
        }
        return changes;
    }

    private NamedResource generateNode(String nodeUriBase, int index) {
        int i = index;
        IRI iri;
        do {
            iri = IRI.create(nodeUriBase + i);
            if (!ontology.containsIndividualInSignature(iri)) {
                return NamedResource.create(iri.toURI());
            }
            i++;
        }
        while (i < (i + NEXT_NODE_GENERATION_THRESHOLD));
        throw new IdentifierGenerationException(
                "Unable to generate identifier for sequence node with base " + nodeUriBase);
    }

    @Override
    public void updateList(ReferencedListValueDescriptor descriptor) {
        // TODO
    }
}
