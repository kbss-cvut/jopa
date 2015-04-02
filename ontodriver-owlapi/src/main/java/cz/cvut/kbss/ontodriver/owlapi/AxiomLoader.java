package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

class AxiomLoader {

    private OWLOntology ontology;
    private OWLDataFactory dataFactory;
    private OWLReasoner reasoner;

    private AxiomDescriptor descriptor;
    private NamedResource subject;
    private Set<URI> inferredAssertions;

    AxiomLoader(OntologyStructures snapshot) {
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.reasoner = snapshot.getReasoner();
    }

    Collection<Axiom<?>> findAxioms(AxiomDescriptor descriptor) {
        this.descriptor = descriptor;
        this.subject = descriptor.getSubject();
        if (!ontology.containsIndividualInSignature(IRI.create(subject.getIdentifier()))) {
            return Collections.emptySet();
        }
        final Collection<Axiom<?>> result = new HashSet<>();
        final Map<URI, Assertion> inferredAssertions = resolveInferredAssertions(descriptor);
        this.inferredAssertions = inferredAssertions.keySet();

        result.addAll(loadInferredValues(inferredAssertions));
        result.addAll(loadExplicitValues(descriptor));
        return result;
    }

    private Map<URI, Assertion> resolveInferredAssertions(AxiomDescriptor descriptor) {
        return descriptor.getAssertions().stream().filter(Assertion::isInferred).collect(
                Collectors.toMap(NamedResource::getIdentifier, a -> a));
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
                    axioms.addAll(owlClassesToAxioms(subject, inferTypes(individual)));
                    break;
                case DATA_PROPERTY:
                    axioms.addAll(inferDataPropertyValues(individual, a));
                    break;
                case OBJECT_PROPERTY:
                    axioms.addAll(inferObjectPropertyValues(individual, a));
                    break;
                case PROPERTY:
                    // When we don't know, try all
                    axioms.addAll(owlClassesToAxioms(subject, inferTypes(individual)));
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
        return dataFactory.getOWLNamedIndividual(
                IRI.create(subject.getIdentifier()));
    }

    private Collection<? extends OWLClassExpression> inferTypes(OWLNamedIndividual individual) {
        return reasoner.getTypes(individual, false).getFlattened();
    }

    private Collection<Axiom<?>> owlClassesToAxioms(NamedResource subject, Collection<? extends OWLClassExpression> classes) {
        final Set<Axiom<?>> axioms = new HashSet<>(classes.size());
        final Assertion assertion = Assertion.createClassAssertion(false);
        classes.stream().forEach(
                cls -> axioms.add(new AxiomImpl<>(subject, assertion, new Value<>(cls.asOWLClass().getIRI().toURI()))));
        return axioms;
    }

    private Collection<Axiom<?>> inferDataPropertyValues(OWLNamedIndividual individual, Assertion dpAssertion) {
        final Set<OWLLiteral> literals = reasoner.getDataPropertyValues(individual, dataProperty(dpAssertion));
        return literals.stream().map(owlLiteral -> new AxiomImpl<>(subject, dpAssertion,
                new Value<>(OwlapiUtils.owlLiteralToValue(owlLiteral)))).collect(Collectors.toSet());
    }

    private OWLDataProperty dataProperty(Assertion dataPropertyAssertion) {
        return dataFactory.getOWLDataProperty(IRI.create(dataPropertyAssertion.getIdentifier()));
    }

    private Collection<Axiom<?>> dataPropertyValuesToAxioms(AxiomDescriptor descriptor, Collection<OWLDataPropertyAssertionAxiom> dpAssertions) {
        final Set<Axiom<?>> axioms = new HashSet<>(dpAssertions.size());
        for (OWLDataPropertyAssertionAxiom assertion : dpAssertions) {
            final Assertion ass = Assertion.createDataPropertyAssertion(
                    assertion.getProperty().asOWLDataProperty().getIRI().toURI(),
                    false);
            if (skipAssertion(ass)) {
                continue;
            }
            axioms.add(new AxiomImpl<>(descriptor.getSubject(), ass,
                    new Value<>(OwlapiUtils.owlLiteralToValue(assertion.getObject()))));
        }
        return axioms;
    }

    private boolean skipAssertion(Assertion assertion) {
        final Assertion unspecifiedTypeAssertion = Assertion.createPropertyAssertion(assertion.getIdentifier(), false);
        return !(descriptor.containsAssertion(assertion) || descriptor.containsAssertion(
                unspecifiedTypeAssertion)) || inferredAssertions.contains(assertion.getIdentifier());
    }

    private Collection<Axiom<?>> inferObjectPropertyValues(OWLNamedIndividual individual, Assertion opAssertion) {
        final Set<OWLNamedIndividual> individuals = reasoner.getObjectPropertyValues(individual,
                objectProperty(opAssertion)).getFlattened();
        return individuals.stream().map(
                target -> new AxiomImpl<>(subject, opAssertion,
                        new Value<>(NamedResource.create(target.getIRI().toURI())))).collect(
                Collectors.toSet());
    }

    private OWLObjectProperty objectProperty(Assertion objectPropertyAssertion) {
        return dataFactory.getOWLObjectProperty(IRI.create(objectPropertyAssertion.getIdentifier()));
    }

    private Collection<Axiom<?>> objectPropertyValuesToAxioms(AxiomDescriptor descriptor, Collection<OWLObjectPropertyAssertionAxiom> opAssertions) {
        final Set<Axiom<?>> axioms = new HashSet<>(opAssertions.size());
        for (OWLObjectPropertyAssertionAxiom assertion : opAssertions) {
            final Assertion ass = Assertion.createObjectPropertyAssertion(
                    assertion.getProperty().asOWLObjectProperty().getIRI().toURI(), false);
            if (skipAssertion(ass)) {
                continue;
            }
            final IRI target = assertion.getObject().asOWLNamedIndividual().getIRI();
            axioms.add(new AxiomImpl<>(descriptor.getSubject(), ass,
                    new Value<>(NamedResource.create(target.toURI()))));
        }
        return axioms;
    }

    private Collection<Axiom<?>> annotationPropertyValuesToAxioms(AxiomDescriptor descriptor, Collection<OWLAnnotationAssertionAxiom> apAssertions) {
        final Set<Axiom<?>> axioms = new HashSet<>(apAssertions.size());
        for (OWLAnnotationAssertionAxiom assertion : apAssertions) {
            final Assertion ass = Assertion.createAnnotationPropertyAssertion(
                    assertion.getProperty().asOWLAnnotationProperty().getIRI().toURI(), false);
            if (skipAssertion(ass)) {
                continue;
            }
            Value<?> val;
            if (assertion.getValue().asIRI().isPresent()) {
                val = new Value<>(assertion.getValue().asIRI().get().toURI());
            } else {
                val = new Value<>(OwlapiUtils.owlLiteralToValue(assertion.getValue().asLiteral().get()));
            }
            axioms.add(new AxiomImpl<>(descriptor.getSubject(), ass, val));
        }
        return axioms;
    }

    private Collection<Axiom<?>> loadExplicitValues(AxiomDescriptor descriptor) {
        final OWLNamedIndividual individual = getIndividual();
        final Collection<Axiom<?>> axioms = new HashSet<>();
        if (!inferredAssertions.contains(URI.create(CommonVocabulary.RDF_TYPE))) {
            final Collection<OWLClassExpression> classes = EntitySearcher.getTypes(individual, ontology);
            axioms.addAll(owlClassesToAxioms(descriptor.getSubject(), classes));
        }
        // TODO This may be inefficient in case there are much more properties for an individual than in the descriptor
        // Perhaps we should use EntitySearcher and look for values of concrete properties
        final Collection<OWLDataPropertyAssertionAxiom> dpAssertions = ontology.getDataPropertyAssertionAxioms(
                individual);
        axioms.addAll(dataPropertyValuesToAxioms(descriptor, dpAssertions));
        final Collection<OWLObjectPropertyAssertionAxiom> opAssertions = ontology.getObjectPropertyAssertionAxioms(
                individual);
        axioms.addAll(objectPropertyValuesToAxioms(descriptor, opAssertions));
        final Collection<OWLAnnotationAssertionAxiom> apAssertions = ontology.getAnnotationAssertionAxioms(
                individual.getIRI());
        axioms.addAll(annotationPropertyValuesToAxioms(descriptor, apAssertions));
        return axioms;
    }
}
