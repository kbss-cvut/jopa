package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class MainAxiomLoader {

    private final OwlapiAdapter adapter;
    private final OntologySnapshot snapshot;

    private OWLOntology ontology;

    private Set<URI> inferredAssertionUris = new HashSet<>();
    private Set<Assertion> inferredAssertions = new HashSet<>();
    private Set<Assertion> explicitAssertions = new HashSet<>();

    MainAxiomLoader(OwlapiAdapter adapter, OntologySnapshot snapshot) {
        this.adapter = adapter;
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
    }

    Collection<Axiom<?>> findAxioms(AxiomDescriptor descriptor) {
        final NamedResource subject = descriptor.getSubject();
        if (!ontology.containsIndividualInSignature(IRI.create(subject.getIdentifier()))) {
            return Collections.emptySet();
        }
        final Collection<Axiom<?>> result = new ArrayList<>();
        resolveInferredAssertions(descriptor);

        result.addAll(loadInferredValues(subject));
        result.addAll(loadExplicitValues(subject));
        return result;
    }

    private void resolveInferredAssertions(AxiomDescriptor descriptor) {
        descriptor.getAssertions().forEach(assertion -> {
            if (assertion.isInferred()) {
                inferredAssertionUris.add(assertion.getIdentifier());
                inferredAssertions.add(assertion);
            } else {
                explicitAssertions.add(assertion);
            }
        });
    }

    private Collection<Axiom<?>> loadInferredValues(NamedResource subject) {
        if (inferredAssertions.isEmpty()) {
            return Collections.emptySet();
        }
        return new InferredAxiomLoader(adapter, snapshot).loadAxioms(subject, inferredAssertions);
    }

    private Collection<Axiom<?>> loadExplicitValues(NamedResource subject) {
        if (explicitAssertions.isEmpty()) {
            return Collections.emptySet();
        }
        final Collection<Axiom<?>> values = new ExplicitAxiomLoader(adapter, snapshot)
                .loadAxioms(subject, explicitAssertions);
        return values.stream().filter(axiom -> !inferredAssertionUris.contains(axiom.getAssertion().getIdentifier()))
                     .collect(Collectors.toList());
    }
}
