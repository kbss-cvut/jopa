/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Translations;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableRemoveAxiom;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class ReferencedListIterator<T> extends OwlapiListIterator<T> {

    final OWLObjectProperty hasNextProperty;
    final OWLProperty hasContentProperty;

    final OWLOntology ontology;
    private final OWLDataFactory dataFactory;

    final AxiomAdapter axiomAdapter;

    OWLIndividual previousNode;
    OWLIndividual currentNode;
    OWLObjectProperty previousNextNodeProperty;
    OWLObjectProperty currentNextNodeProperty;
    Collection<? extends OWLObject> nextItem;

    final ReferencedListDescriptor descriptor;

    ReferencedListIterator(ReferencedListDescriptor descriptor, OntologySnapshot snapshot, AxiomAdapter axiomAdapter) {
        this.ontology = snapshot.getOntology();
        this.dataFactory = snapshot.getDataFactory();
        this.descriptor = descriptor;
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(descriptor.getNextNode().getIdentifier()));
        this.hasContentProperty = assertionToOwlProperty(descriptor.getNodeContent());
        this.axiomAdapter = axiomAdapter;
        this.currentNextNodeProperty = dataFactory
                .getOWLObjectProperty(IRI.create(descriptor.getListProperty().getIdentifier()));
        this.previousNextNodeProperty = currentNextNodeProperty;
        this.currentNode = OwlapiUtils.getIndividual(descriptor.getListOwner(), dataFactory);
        this.previousNode = currentNode;
    }

    private OWLProperty assertionToOwlProperty(Assertion a) {
        return switch (a.getType()) {
            case OBJECT_PROPERTY -> dataFactory
                    .getOWLObjectProperty(IRI.create(descriptor.getNodeContent().getIdentifier()));
            case DATA_PROPERTY ->
                    dataFactory.getOWLDataProperty(IRI.create(descriptor.getNodeContent().getIdentifier()));
            default -> throw new IllegalArgumentException("Node content property cannot be type " + a.getType());
        };
    }

    @Override
    public boolean hasNext() {
        return EntitySearcher.getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology)
                             .anyMatch(e -> true);
    }

    void doStep() {
        final Collection<OWLIndividual> nextNodes =
                EntitySearcher.getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology)
                              .collect(Collectors.toSet());
        if (nextNodes.isEmpty()) {
            this.nextItem = Collections.emptyList();
            return;
        }
        checkMaxSuccessors(currentNextNodeProperty, nextNodes);
        this.previousNextNodeProperty = currentNextNodeProperty;
        this.currentNextNodeProperty = hasNextProperty; // This just switches from hasList to hasNext
        final OWLIndividual node = nextNodes.iterator().next();
        checkIsNamed(node);
        this.previousNode = currentNode;
        this.currentNode = node;
        this.nextItem = resolveCurrentContent();
    }

    private Collection<? extends OWLObject> resolveCurrentContent() {
        if (hasContentProperty.isOWLObjectProperty()) {
            return EntitySearcher.getObjectPropertyValues(currentNode, hasContentProperty.asOWLObjectProperty(), ontology)
                                 .collect(Collectors.toSet());
        } else {
            assert hasContentProperty.isOWLDataProperty();
            return EntitySearcher.getDataPropertyValues(currentNode, hasContentProperty.asOWLDataProperty(), ontology)
                                 .collect(Collectors.toSet());
        }
    }

    @Override
    public Axiom<T> next() {
        final T value = nextValue();
        return axiomAdapter
                .createAxiom(NamedResource.create(currentNode.asOWLNamedIndividual().getIRI().toURI()),
                        descriptor.getNodeContent(), value);
    }

    @Override
    T nextValue() {
        doStep();
        if (nextItem.isEmpty()) {
            throw new NoSuchElementException("There are no more elements.");
        }
        return extractNodeContent();
    }

    protected T extractNodeContent() {
        verifyContentValueCount(nextItem);
        if (nextItem.size() == 1) {
            final OWLObject value = nextItem.iterator().next();
            if (value.isIndividual()) {
                final OWLIndividual individual = (OWLIndividual) value;
                checkIsNamed(individual);
                return (T) NamedResource.create(individual.asOWLNamedIndividual().getIRI().toURI());
            } else {
                final OWLLiteral literal = (OWLLiteral) value;
                return (T) OwlapiUtils.owlLiteralToValue(literal);
            }
        } else {
            final Translations translations = new Translations();
            nextItem.forEach(n -> {
                assert n instanceof OWLLiteral;
                final OWLLiteral lit = (OWLLiteral) n;
                assert lit.getLang() != null;
                translations.set(lit.getLang(), lit.getLiteral());
            });
            return (T) translations;
        }
    }

    private void verifyContentValueCount(Collection<? extends OWLObject> values) {
        if (values.isEmpty()) {
            throw icViolatedException(currentNode.toStringID(), 0);
        }
        if (values.size() == 1) {
            return;
        }
        final Set<String> langs = new HashSet<>();
        for (OWLObject s : values) {
            if (s.isIndividual()) {
                throw icViolatedException(currentNode.toStringID(), values.size());
            }
            assert s instanceof OWLLiteral;
            final OWLLiteral literal = (OWLLiteral) s;
            if (literal.getLang() != null && !langs.contains(literal.getLang())) {
                langs.add(literal.getLang());
            } else {
                throw icViolatedException(currentNode.toStringID(), values.size());
            }
        }
    }

    private static IntegrityConstraintViolatedException icViolatedException(String nodeId, int count) {
        return new IntegrityConstraintViolatedException("Expected exactly one content statement for node <" + nodeId + ">, but got " + count);
    }

    @Override
    public NamedResource getCurrentNode() {
        return NamedResource.create(currentNode.asOWLNamedIndividual().getIRI().toURI());
    }

    @Override
    List<TransactionalChange> removeWithoutReconnect() {
        final MutableRemoveAxiom removeFromPrevious = new MutableRemoveAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(previousNextNodeProperty, previousNode, currentNode));
        final OWLIndividual nextNode = getNextNode();
        if (nextNode != null) {
            return List.of(removeFromPrevious, new MutableRemoveAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(currentNextNodeProperty, currentNode, nextNode)));
        }
        return List.of(removeFromPrevious);
    }

    private OWLIndividual getNextNode() {
        final Stream<OWLIndividual> nextOnes =
                EntitySearcher.getObjectPropertyValues(currentNode, currentNextNodeProperty, ontology);
        final Iterator<OWLIndividual> it = nextOnes.iterator();
        return it.hasNext() ? it.next() : null;
    }

    @Override
    List<TransactionalChange> replaceNode(T newValue) {
        // We know there is exactly one, because nextItem has to have been called before this method
        final List<TransactionalChange> changes;
        if (hasContentProperty.isOWLObjectProperty()) {
            assert nextItem.size() == 1;
            final OWLObject originalContent = nextItem.iterator().next();
            final OWLNamedIndividual newContent = OwlapiUtils.getIndividual((NamedResource) newValue, dataFactory);
            changes = List.of(
                    new MutableRemoveAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasContentProperty.asOWLObjectProperty(), currentNode, (OWLIndividual) originalContent)),
                    new MutableAddAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasContentProperty.asOWLObjectProperty(), currentNode, newContent)));
        } else {
            changes = replaceDataPropertyContent(newValue);
        }
        return changes;
    }

    private List<TransactionalChange> replaceDataPropertyContent(T newValue) {
        final List<TransactionalChange> result = new ArrayList<>(nextItem.size() + 1);
        nextItem.forEach(o -> result.add(new MutableRemoveAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(hasContentProperty.asOWLDataProperty(), currentNode, (OWLLiteral) o))));
        final ReferencedListNodeGenerator nodeGenerator = new ReferencedListNodeGenerator(descriptor, axiomAdapter, ontology);
        result.addAll(nodeGenerator.generateNodeContent(getCurrentNode(), newValue));
        return result;
    }
}
