/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.owlapi.container;

import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
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
        this.axiomAdapter = new AxiomAdapter(snapshot.dataFactory());
        this.snapshot = snapshot;
        this.ontology = snapshot.ontology();
        this.dataFactory = snapshot.dataFactory();
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
        Objects.requireNonNull(descriptor);
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
                          .forEach((lp, value) -> lit.add(new PropertyValuePair<>(lp.asOWLDataProperty()
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
        if (includeInferred && snapshot.reasoner() != null) {
            containerCandidates = snapshot.reasoner().getObjectPropertyValues(owner, property).entities().toList();
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

    /**
     * Creates a new container and fills it with the specified values.
     *
     * @param descriptor Container value descriptor
     * @param <T>        Type of container values
     */
    public <T> void persistContainer(ContainerValueDescriptor<T> descriptor) {
        Objects.requireNonNull(descriptor);
        if (descriptor.getValues().isEmpty()) {
            return;
        }
        final OWLNamedIndividual owner = dataFactory.getOWLNamedIndividual(IRI.create(descriptor.getOwner()
                                                                                                .getIdentifier()));
        final List<TransactionalChange> changes = new ArrayList<>(descriptor.getValues().size() + 1);
        final OWLNamedIndividual container = createContainer(owner, descriptor.getProperty(), descriptor.getType(), changes);
        changes.addAll(createContainerContent(container, descriptor.getProperty(), descriptor.getValues()));
        owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(changes));
    }

    private OWLNamedIndividual createContainer(OWLNamedIndividual owner, Assertion property, URI containerType,
                                               List<TransactionalChange> changes) {
        final OWLNamedIndividual container = dataFactory.getOWLNamedIndividual(IRI.create(owlapiAdapter.generateIdentifier(URI.create(RDFS.CONTAINER))));
        final OWLObjectProperty containerOwnerProperty = dataFactory.getOWLObjectProperty(IRI.create(property.getIdentifier()));
        changes.add(new MutableAddAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(containerOwnerProperty, owner, container)));
        changes.add(new MutableAddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(dataFactory.getOWLClass(IRI.create(containerType)), container)));
        return container;
    }

    private <T> List<MutableAddAxiom> createContainerContent(OWLIndividual container,
                                                             Assertion property,
                                                             List<T> values) {
        final List<MutableAddAxiom> result = new ArrayList<>();
        for (int i = 0; i < values.size(); i++) {
            final T value = values.get(i);
            final IRI propertyIri = IRI.create(MEMBERSHIP_PROPERTY_URI_BASE + (i + 1));
            final OWLAxiom assertionAxiom = switch (property.getType()) {
                case OBJECT_PROPERTY ->
                        dataFactory.getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(propertyIri), container, dataFactory.getOWLNamedIndividual(IRI.create(value.toString())));
                case DATA_PROPERTY ->
                        dataFactory.getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(propertyIri), container, OwlapiUtils.createOWLLiteralFromValue(value, property.getLanguage()));
                default -> throw new IllegalArgumentException("Unsupported property type " + property.getType());
            };
            result.add(new MutableAddAxiom(ontology, assertionAxiom));
        }
        return result;
    }

    /**
     * Updates the content of an existing container corresponding to the specified descriptor.
     * <p>
     * If the descriptor has no value, the container is removed completely.
     *
     * @param descriptor Descriptor with new container values
     * @param <T>        Value type
     * @throws OwlapiDriverException If an error accessing the container occurs
     */
    public <T> void updateContainer(ContainerValueDescriptor<T> descriptor) throws OwlapiDriverException {
        Objects.requireNonNull(descriptor);
        final Optional<? extends OWLIndividual> container = findContainer(descriptor, false);
        if (container.isEmpty()) {
            persistContainer(descriptor);
        } else {
            final List<TransactionalChange> containerUpdate = new ArrayList<>(clearContainer(container.get(), descriptor.getProperty()));
            containerUpdate.addAll(createContainerContent(container.get(), descriptor.getProperty(), descriptor.getValues()));
            if (descriptor.getValues().isEmpty()) {
                containerUpdate.addAll(removeContainer(descriptor.getOwner(), descriptor.getProperty(), container.get()));
            }
            owlapiAdapter.addTransactionalChanges(snapshot.applyChanges(containerUpdate));
        }
    }

    private List<MutableRemoveAxiom> clearContainer(OWLIndividual container, Assertion property) {
        final List<MutableRemoveAxiom> removeAxioms = new ArrayList<>(switch (property.getType()) {
            case OBJECT_PROPERTY -> EntitySearcher.getObjectPropertyValues(container, ontology).entries().stream()
                                                  .filter(e -> e.getKey().getNamedProperty().getIRI().getIRIString()
                                                                .startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                                                  .map(e -> new MutableRemoveAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(e.getKey(), container, e.getValue())))
                                                  .toList();
            case DATA_PROPERTY -> EntitySearcher.getDataPropertyValues(container, ontology).entries().stream()
                                                .filter(e -> e.getKey().asOWLDataProperty().getIRI().getIRIString()
                                                              .startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                                                .map(e -> new MutableRemoveAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(e.getKey(), container, e.getValue())))
                                                .toList();
            default -> throw new IllegalArgumentException("Unsupported property type " + property.getType());
        });
        if (container.isAnonymous()) {
            EntitySearcher.getAnnotationAssertionAxioms(container.asOWLAnonymousIndividual(), ontology)
                          .filter(a -> a.getProperty().getIRI().getIRIString().startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                          .map(aaa -> new MutableRemoveAxiom(ontology, aaa)).forEach(removeAxioms::add);
        } else {
            EntitySearcher.getAnnotationAssertionAxioms(container.asOWLNamedIndividual(), ontology)
                          .filter(a -> a.getProperty().getIRI().getIRIString().startsWith(MEMBERSHIP_PROPERTY_URI_BASE))
                          .map(aaa -> new MutableRemoveAxiom(ontology, aaa)).forEach(removeAxioms::add);
        }
        return removeAxioms;
    }

    private List<MutableRemoveAxiom> removeContainer(NamedResource owner, Assertion property, OWLIndividual container) {
        final OWLNamedIndividual ownerIndividual = dataFactory.getOWLNamedIndividual(IRI.create(owner.getIdentifier()));
        final List<MutableRemoveAxiom> toRemove = new ArrayList<>(List.of(
                new MutableRemoveAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(IRI.create(property.getIdentifier())), ownerIndividual, container))
        ));
        ontology.classAssertionAxioms(container).forEach(caa -> toRemove.add(new MutableRemoveAxiom(ontology, caa)));
        return toRemove;
    }
}
