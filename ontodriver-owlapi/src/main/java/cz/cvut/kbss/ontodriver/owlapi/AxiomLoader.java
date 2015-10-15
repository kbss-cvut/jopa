package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class AxiomLoader {

    // TODO Refactor this class

    private OwlapiAdapter adapter;

    private OWLOntology ontology;
    private OWLDataFactory dataFactory;
    private OWLReasoner reasoner;

    private AxiomDescriptor descriptor;
    private NamedResource subject;
    private Set<URI> inferredAssertions = new HashSet<>();
    private Set<URI> explicitAssertions = new HashSet<>();

    private AxiomAdapter axiomAdapter;

    AxiomLoader(OwlapiAdapter adapter, OntologyStructures snapshot) {
        this.adapter = adapter;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.reasoner = snapshot.getReasoner();
        this.axiomAdapter = new AxiomAdapter(dataFactory, adapter.getLanguage());
    }

    Collection<Axiom<?>> findAxioms(AxiomDescriptor descriptor) {
        this.descriptor = descriptor;
        this.subject = descriptor.getSubject();
        if (!ontology.containsIndividualInSignature(IRI.create(subject.getIdentifier()))) {
            return Collections.emptySet();
        }
        final Collection<Axiom<?>> result = new ArrayList<>();
        final Map<URI, Assertion> inferredAssertions = resolveInferredAssertions(descriptor);

        result.addAll(loadInferredValues(inferredAssertions));
        result.addAll(loadExplicitValues(descriptor));
        return result;
    }

    private Map<URI, Assertion> resolveInferredAssertions(AxiomDescriptor descriptor) {
        final Map<URI, Assertion> inferred = new HashMap<>(descriptor.getAssertions().size());
        descriptor.getAssertions().forEach(assertion -> {
            if (assertion.isInferred()) {
                inferredAssertions.add(assertion.getIdentifier());
                inferred.put(assertion.getIdentifier(), assertion);
            } else {
                explicitAssertions.add(assertion.getIdentifier());
            }
        });
        return inferred;
    }

    private Collection<Axiom<?>> loadInferredValues(Map<URI, Assertion> assertions) {
        if (assertions.isEmpty()) {
            return Collections.emptySet();
        }
        if (reasoner == null) {
            throw new ReasonerNotAvailableException();
        }
        reasoner.flush();
        final OWLNamedIndividual individual = getIndividual();
        final Collection<Axiom<?>> axioms = new HashSet<>();
        for (Assertion a : assertions.values()) {
            switch (a.getType()) {
                case CLASS:
                    axioms.addAll(adapter.getTypesHandler().getTypes(subject, null, true));
                    break;
                case DATA_PROPERTY:
                    axioms.addAll(inferDataPropertyValues(individual, a));
                    break;
                case OBJECT_PROPERTY:
                    axioms.addAll(inferObjectPropertyValues(individual, a));
                    break;
                case PROPERTY:
                    // When we don't know, try all
                    axioms.addAll(adapter.getTypesHandler().getTypes(subject, null, true));
                    axioms.addAll(inferDataPropertyValues(individual, a));
                    axioms.addAll(inferObjectPropertyValues(individual, a));
                    break;
                default:
                    break;
            }
        }
        return axioms;
    }

    private OWLNamedIndividual getIndividual() {
        return dataFactory.getOWLNamedIndividual(IRI.create(subject.getIdentifier()));
    }

    private Collection<Axiom<?>> inferDataPropertyValues(OWLNamedIndividual individual, Assertion dpAssertion) {
        final Set<OWLLiteral> literals = reasoner.getDataPropertyValues(individual, dataProperty(dpAssertion));
        return literals.stream().map(owlLiteral -> new AxiomImpl<>(subject, dpAssertion,
                new Value<>(OwlapiUtils.owlLiteralToValue(owlLiteral)))).collect(Collectors.toSet());
    }

    private OWLDataProperty dataProperty(Assertion dataPropertyAssertion) {
        return dataFactory.getOWLDataProperty(IRI.create(dataPropertyAssertion.getIdentifier()));
    }

    private Collection<Axiom<?>> dataPropertyValuesToAxioms(Collection<OWLDataPropertyAssertionAxiom> dpAssertions) {
        final Set<Axiom<?>> axioms = new HashSet<>(dpAssertions.size());
        for (OWLDataPropertyAssertionAxiom assertion : dpAssertions) {
            if (skipAssertion(assertion.getProperty().asOWLDataProperty().getIRI().toURI())) {
                continue;
            }
            axioms.add(axiomAdapter.toAxiom(subject, assertion, false));
        }
        return axioms;
    }

    private boolean skipAssertion(URI assertionUri) {
        final Assertion unspecifiedTypeAssertion = Assertion.createUnspecifiedPropertyAssertion(false);
        return !(explicitAssertions.contains(assertionUri) || descriptor.containsAssertion(
                unspecifiedTypeAssertion)) || inferredAssertions.contains(assertionUri);
    }

    private Collection<Axiom<?>> inferObjectPropertyValues(OWLNamedIndividual individual, Assertion opAssertion) {
        final Set<OWLNamedIndividual> individuals = reasoner.getObjectPropertyValues(individual,
                objectProperty(opAssertion)).getFlattened();
        return individuals.stream().map(
                target -> axiomAdapter.createAxiom(subject, opAssertion, NamedResource.create(target.getIRI().toURI())))
                          .collect(
                                  Collectors.toList());
    }

    private OWLObjectProperty objectProperty(Assertion objectPropertyAssertion) {
        return dataFactory.getOWLObjectProperty(IRI.create(objectPropertyAssertion.getIdentifier()));
    }

    private Collection<Axiom<?>> objectPropertyValuesToAxioms(
            Collection<OWLObjectPropertyAssertionAxiom> opAssertions) {
        final Set<Axiom<?>> axioms = new HashSet<>(opAssertions.size());
        for (OWLObjectPropertyAssertionAxiom assertion : opAssertions) {
            if (skipAssertion(assertion.getProperty().asOWLObjectProperty().getIRI().toURI())) {
                continue;
            }
            axioms.add(axiomAdapter.toAxiom(subject, assertion, false));
        }
        return axioms;
    }

    private Collection<Axiom<?>> annotationPropertyValuesToAxioms(
            Collection<OWLAnnotationAssertionAxiom> apAssertions) {
        final Set<Axiom<?>> axioms = new HashSet<>(apAssertions.size());
        for (OWLAnnotationAssertionAxiom assertion : apAssertions) {
            if (skipAssertion(assertion.getProperty().asOWLAnnotationProperty().getIRI().toURI())) {
                continue;
            }
            axioms.add(axiomAdapter.toAxiom(subject, assertion, false));
        }
        return axioms;
    }

    private Collection<Axiom<?>> loadExplicitValues(AxiomDescriptor descriptor) {
        final OWLNamedIndividual individual = getIndividual();
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        if (explicitAssertions.contains(Assertion.createClassAssertion(false).getIdentifier())) {
            axioms.addAll(adapter.getTypesHandler().getTypes(descriptor.getSubject(), null, false));
        }
        // This may be inefficient in case there are much more properties for an individual than in the descriptor
        // Perhaps we should use EntitySearcher and look for values of concrete properties
        final Collection<OWLDataPropertyAssertionAxiom> dpAssertions = ontology.getDataPropertyAssertionAxioms(
                individual);
        axioms.addAll(dataPropertyValuesToAxioms(dpAssertions));
        final Collection<OWLObjectPropertyAssertionAxiom> opAssertions = ontology.getObjectPropertyAssertionAxioms(
                individual);
        axioms.addAll(objectPropertyValuesToAxioms(opAssertions));
        final Collection<OWLAnnotationAssertionAxiom> apAssertions = ontology.getAnnotationAssertionAxioms(
                individual.getIRI());
        axioms.addAll(annotationPropertyValuesToAxioms(apAssertions));
        return axioms;
    }
}
