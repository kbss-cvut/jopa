package cz.cvut.kbss.ontodriver.owlapi.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

public class ContainerHandler {

    private static final String MEMBERSHIP_PROPERTY_URI_BASE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#_";
    private static final int MEMBERSHIP_PROPERTY_URI_BASE_LENGTH = MEMBERSHIP_PROPERTY_URI_BASE.length();

    private final OwlapiAdapter owlapiAdapter;
    private final AxiomAdapter axiomAdapter;

    private final OntologySnapshot snapshot;
    private final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    public ContainerHandler(OwlapiAdapter owlapiAdapter, OntologySnapshot snapshot) {
        this.owlapiAdapter = owlapiAdapter;
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory());
        this.snapshot = snapshot;
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
    }

    /**
     * Reads the content of a container corresponding to the specified description.
     * <p>
     * The container values are returned as a list of axioms where the owner of the container points to the individual
     * values.
     *
     * @param descriptor Container description
     * @return List of axioms representing the container's content
     * @throws cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException If an error accessing the container
     *                                                                        occurs
     */
    public List<Axiom<?>> readContainer(ContainerDescriptor descriptor) throws OwlapiDriverException {
        final boolean includeInferred = descriptor.getProperty().isInferred();
        final Optional<? extends OWLIndividual> container = findContainer(descriptor, includeInferred);
        if (container.isEmpty()) {
            return List.of();
        }
        if (descriptor.getProperty().getType() == Assertion.AssertionType.OBJECT_PROPERTY) {
            final List<PropertyValuePair<OWLIndividual>> ind = new ArrayList<>();
            EntitySearcher.getObjectPropertyValues(container.get(), ontology)
                          .forEach((op, value) -> ind.add(new PropertyValuePair<>(op.getNamedProperty()
                                                                                    .getIRI(), value)));
            ind.sort((p1, p2) -> containerElementIriComparator(p1.property, p2.property));
            final List<Axiom<?>> result = new ArrayList<>(ind.size());
            ind.forEach(pv -> result.add(axiomAdapter.createAxiom(descriptor.getOwner(), descriptor.getProperty(), NamedResource.create(pv.value.asOWLNamedIndividual()
                                                                                                                                                .getIRI()
                                                                                                                                                .toURI()))));
            return result;
        } else {
            final List<Axiom<?>> result = new ArrayList<>();
            final List<PropertyValuePair<OWLLiteral>> lit = new ArrayList<>();
            EntitySearcher.getDataPropertyValues(container.get(), ontology)
                          .forEach((lp, value) -> lit.add(new PropertyValuePair<>(lp.asOWLObjectProperty()
                                                                                    .getIRI(), value)));
            lit.sort((p1, p2) -> containerElementIriComparator(p1.property, p2.property));
            lit.forEach(pv -> result.add(axiomAdapter.createAxiom(descriptor.getOwner(), descriptor.getProperty(), OwlapiUtils.owlLiteralToValue(pv.value))));
            if (result.isEmpty()) {
                return readContainerElementsAsAnnotationPropertyValues(container.get(), descriptor.getOwner(), descriptor.getProperty());
            }
            return result;
        }
    }

    private List<Axiom<?>> readContainerElementsAsAnnotationPropertyValues(OWLIndividual container, NamedResource owner,
                                                                           Assertion property) {
        // For some reason a call to EntitySearcher with ternary operator on container does not compile
        final Stream<OWLAnnotation> annotations = container.isAnonymous() ? EntitySearcher.getAnnotations(container.asOWLAnonymousIndividual(), ontology) : EntitySearcher.getAnnotations(container.asOWLNamedIndividual(), ontology);
        return annotations.filter(a -> a.getProperty().getIRI().getIRIString()
                                        .startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                          .sorted((a1, a2) -> containerElementIriComparator(a1.getProperty()
                                                                              .getIRI(), a2.getProperty().getIRI()))
                          .map(an -> {
                              if (an.getValue().isLiteral()) {
                                  assert an.getValue().asLiteral().isPresent();
                                  return axiomAdapter.createAxiom(owner, property, OwlapiUtils.owlLiteralToValue(an.getValue()
                                                                                                                   .asLiteral()
                                                                                                                   .get()));
                              } else if (an.getValue().isIRI()) {
                                  assert an.getValue().asIRI().isPresent();
                                  return axiomAdapter.createAxiom(owner, property, an.getValue().asIRI().get());
                              } else {
                                  return null;
                              }
                          }).filter(Objects::nonNull).toList();
    }

    private Optional<? extends OWLIndividual> findContainer(ContainerDescriptor descriptor,
                                                            boolean includeInferred) throws OwlapiDriverException {
        final OWLNamedIndividual owner = dataFactory.getOWLNamedIndividual(IRI.create(descriptor.getOwner()
                                                                                                .getIdentifier()));
        final OWLObjectProperty property = dataFactory.getOWLObjectProperty(IRI.create(descriptor.getProperty()
                                                                                                 .getIdentifier()));
        final List<? extends OWLIndividual> containerCandidates;
        if (includeInferred && snapshot.getReasoner() != null) {
            containerCandidates = snapshot.getReasoner().getObjectPropertyValues(owner, property).entities().toList();
        } else {
            containerCandidates = EntitySearcher.getObjectPropertyValues(owner, property, ontology)
                                                .toList();
        }
        if (containerCandidates.isEmpty()) {
            return getContainerAsAnnotationPropertyValue(descriptor, owner, property);
        }
        if (containerCandidates.size() > 1) {
            throw new IntegrityConstraintViolatedException("Expected a single value of property <" + property + ">, but got multiple.");
        }
        return Optional.of(containerCandidates.get(0));
    }

    private Optional<? extends OWLIndividual> getContainerAsAnnotationPropertyValue(ContainerDescriptor descriptor,
                                                                                    OWLNamedIndividual owner,
                                                                                    OWLObjectProperty property) throws OwlapiDriverException {
        final List<OWLAnnotation> ans = EntitySearcher.getAnnotations(owner, ontology, dataFactory.getOWLAnnotationProperty(IRI.create(descriptor.getProperty()
                                                                                                                                                 .getIdentifier())))
                                                      .toList();
        if (ans.isEmpty()) {
            return Optional.empty();
        }
        if (ans.size() > 1) {
            throw new IntegrityConstraintViolatedException("Expected a single value of property <" + property + ">, but got multiple.");
        }
        final OWLAnnotationValue val = ans.get(0).getValue();
        if (val.isLiteral()) {
            throw new OwlapiDriverException("Expected an individual as value of property <" + property + ">, but got a literal.");
        }
        return val.isIRI() ? val.asIRI().map(dataFactory::getOWLNamedIndividual) : val.asAnonymousIndividual();
    }

    private record PropertyValuePair<T>(IRI property, T value) {}

    private static int containerElementIriComparator(IRI s1, IRI s2) {
        try {
            final int p1Number = Integer.parseInt(s1.getIRIString().substring(MEMBERSHIP_PROPERTY_URI_BASE_LENGTH));
            final int p2Number = Integer.parseInt(s2.getIRIString().substring(MEMBERSHIP_PROPERTY_URI_BASE_LENGTH));
            return p1Number - p2Number;
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Unable to determine container membership property number.", e);
        }
    }
}
