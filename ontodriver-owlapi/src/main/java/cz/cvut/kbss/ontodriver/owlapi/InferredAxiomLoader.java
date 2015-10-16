package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.*;
import java.util.stream.Collectors;

public class InferredAxiomLoader implements AxiomLoader {

    private final OWLReasoner reasoner;
    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    private final OwlapiAdapter adapter;
    private final AxiomAdapter axiomAdapter;

    private NamedResource subject;

    InferredAxiomLoader(OwlapiAdapter adapter, OntologyStructures snapshot) {
        this.adapter = adapter;
        this.reasoner = snapshot.getReasoner();
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory(), adapter.getLanguage());
    }

    @Override
    public Collection<Axiom<?>> loadAxioms(NamedResource subject, Set<Assertion> assertions) {
        this.subject = subject;
        if (assertions.isEmpty()) {
            return Collections.emptySet();
        }
        if (reasoner == null) {
            throw new ReasonerNotAvailableException();
        }
        reasoner.flush();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        final Collection<Axiom<?>> axioms = new HashSet<>();
        for (Assertion a : assertions) {
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

    private Collection<Axiom<?>> inferDataPropertyValues(OWLNamedIndividual individual, Assertion dpAssertion) {
        final Set<OWLLiteral> literals = reasoner.getDataPropertyValues(individual, dataProperty(dpAssertion));
        return literals.stream().map(owlLiteral -> new AxiomImpl<>(subject, dpAssertion,
                new Value<>(OwlapiUtils.owlLiteralToValue(owlLiteral)))).collect(Collectors.toSet());
    }

    private OWLDataProperty dataProperty(Assertion dataPropertyAssertion) {
        return dataFactory.getOWLDataProperty(IRI.create(dataPropertyAssertion.getIdentifier()));
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

    @Override
    public Collection<Axiom<?>> loadPropertyAxioms(NamedResource subject) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final OWLNamedIndividual individual = OwlapiUtils.getIndividual(subject, dataFactory);
        for (OWLDataProperty dp : ontology.getDataPropertiesInSignature()) {
            final Set<OWLLiteral> values = reasoner.getDataPropertyValues(individual, dp);
            for (OWLLiteral literal : values) {
                axioms.add(axiomAdapter.createAxiom(subject,
                        Assertion.createDataPropertyAssertion(dp.getIRI().toURI(), true), literal));
            }
        }
        for (OWLObjectProperty op : ontology.getObjectPropertiesInSignature()) {
            final Set<OWLNamedIndividual> values = reasoner.getObjectPropertyValues(individual, op).getFlattened();
            for (OWLNamedIndividual ind : values) {
                axioms.add(axiomAdapter.createAxiom(subject,
                        Assertion.createObjectPropertyAssertion(op.getIRI().toURI(), true), NamedResource.create(
                                ind.getIRI().toURI())));
            }
        }
        return axioms;
    }
}
