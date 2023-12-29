package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

class ReferencedListNodeGenerator {

    private final String baseUri;
    private final List<TransactionalChange> changes = new ArrayList<>();
    private final ReferencedListDescriptor descriptor;
    private final AxiomAdapter axiomAdapter;
    private final OWLOntology ontology;

    private int index = 0;

    ReferencedListNodeGenerator(ReferencedListDescriptor descriptor, AxiomAdapter axiomAdapter, OWLOntology ontology) {
        this.baseUri = descriptor.getListOwner().getIdentifier().toString() + "-SEQ_";
        this.descriptor = descriptor;
        this.axiomAdapter = axiomAdapter;
        this.ontology = ontology;
    }

    void setIndex(int index) {
        this.index = index;
    }

    List<TransactionalChange> getChanges() {
        return changes;
    }

    <V> NamedResource addListNode(NamedResource previousNode, V value) {
        final Assertion hasNext = index == 0 ? descriptor.getListProperty() : descriptor.getNextNode();
        final NamedResource node = generateNode();
        final OWLAxiom nodeAxiom = axiomAdapter
                .toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(previousNode, hasNext, new Value<>(node)));
        changes.add(new MutableAddAxiom(ontology, nodeAxiom));
        changes.addAll(generateNodeContent(node, value));
        index++;
        return node;
    }

    <V> Collection<TransactionalChange> generateNodeContent(NamedResource node, V value) {
        if (descriptor.getNodeContent().getType() == Assertion.AssertionType.OBJECT_PROPERTY) {
            final OWLAxiom valueAxiom = axiomAdapter
                    .toOwlObjectPropertyAssertionAxiom(new AxiomImpl<>(node, descriptor.getNodeContent(), new Value<>(value)));
            return List.of(new MutableAddAxiom(ontology, valueAxiom));
        } else {
            assert descriptor.getNodeContent().getType() == Assertion.AssertionType.DATA_PROPERTY;
            if (value instanceof MultilingualString) {
                final MultilingualString mls = (MultilingualString) value;
                return mls.getValue().entrySet().stream().map(e -> {
                    final String lang = e.getKey();
                    final String val = e.getValue();
                    final OWLAxiom valueAxiom = axiomAdapter.toOwlDataPropertyAssertionAxiom(new AxiomImpl<>(node, descriptor.getNodeContent(), new Value<>(new LangString(val, lang))));
                    return new MutableAddAxiom(ontology, valueAxiom);
                }).collect(Collectors.toList());
            }
            final OWLAxiom valueAxiom = axiomAdapter.toOwlDataPropertyAssertionAxiom(new AxiomImpl<>(node, descriptor.getNodeContent(), new Value<>(value)));
            return List.of(new MutableAddAxiom(ontology, valueAxiom));
        }
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
        while (i < (i + ReferencedListHandler.NEXT_NODE_GENERATION_THRESHOLD));
        throw new IdentifierGenerationException("Unable to generate identifier for sequence node with base " + baseUri);
    }
}
