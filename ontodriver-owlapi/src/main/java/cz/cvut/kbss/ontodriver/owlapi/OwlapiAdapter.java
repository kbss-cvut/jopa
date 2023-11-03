/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.list.ListHandler;
import cz.cvut.kbss.ontodriver.owlapi.query.OwlapiPreparedStatement;
import cz.cvut.kbss.ontodriver.owlapi.query.OwlapiStatement;
import cz.cvut.kbss.ontodriver.owlapi.query.StatementExecutorFactory;
import cz.cvut.kbss.ontodriver.owlapi.util.IdentifierGenerator;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * Adapter between OntoDriver API and OWLAPI.
 */
public class OwlapiAdapter {

    private final Connector connector;
    private OntologySnapshot ontologySnapshot;

    private StatementExecutorFactory statementExecutorFactory;

    private TransactionState transactionState = TransactionState.INITIAL;
    private List<TransactionalChange> pendingChanges = new ArrayList<>();

    private enum TransactionState {
        INITIAL, RUNNING
    }

    public OwlapiAdapter(Connector connector) {
        this.connector = connector;
    }

    private void startTransactionIfNotActive() {
        if (transactionState == TransactionState.INITIAL) {
            this.ontologySnapshot = connector.getOntologySnapshot();
            this.transactionState = TransactionState.RUNNING;
            this.statementExecutorFactory = new StatementExecutorFactory(ontologySnapshot, connector);
        }
    }

    void commit() {
        if (transactionState != TransactionState.RUNNING) {
            return;
        }
        if (!pendingChanges.isEmpty()) {
            connector.applyChanges(pendingChanges);
            this.pendingChanges = new ArrayList<>();
        }
        transactionCleanup();
    }

    private void transactionCleanup() {
        connector.closeSnapshot(ontologySnapshot);
        this.ontologySnapshot = null;
        this.transactionState = TransactionState.INITIAL;
    }

    void rollback() {
        if (transactionState != TransactionState.RUNNING) {
            return;
        }
        if (!pendingChanges.isEmpty()) {
            pendingChanges = new ArrayList<>();
        }
        transactionCleanup();
    }

    boolean isConsistent(URI context) {
        startTransactionIfNotActive();

        return reasoner().isConsistent();
    }

    private OWLReasoner reasoner() {
        return ontologySnapshot.getReasoner();
    }

    private OWLOntology ontology() {
        return ontologySnapshot.getOntology();
    }

    private OWLDataFactory dataFactory() {
        return ontologySnapshot.getDataFactory();
    }

    List<URI> getContexts() {
        startTransactionIfNotActive();
        return Collections.singletonList(connector.getOntologyUri());
    }

    boolean containsAxiom(Axiom<?> axiom, Set<URI> contexts) {
        startTransactionIfNotActive();
        final Collection<OWLAxiom> owlAxiom = asOwlAxioms(axiom);
        boolean contains;
        for (OWLAxiom ax : owlAxiom) {
            if (axiom.getAssertion().isInferred()) {
                contains = reasoner().isEntailed(ax);
            } else {
                contains = ontology().containsAxiom(ax);
            }
            if (contains) {
                return true;
            }
        }
        return false;
    }

    boolean isInferred(Axiom<?> axiom, Set<URI> contexts) {
        startTransactionIfNotActive();
        final Collection<OWLAxiom> owlAxiom = asOwlAxioms(axiom);
        reasoner().flush();
        return owlAxiom.stream().anyMatch(a -> reasoner().isEntailed(a) && !ontology().containsAxiom(a));
    }

    private Collection<OWLAxiom> asOwlAxioms(Axiom<?> axiom) {
        final Collection<OWLAxiom> owlAxioms = new ArrayList<>(3);
        final AxiomAdapter axiomAdapter = new AxiomAdapter(dataFactory());
        switch (axiom.getAssertion().getType()) {
            case CLASS:
                owlAxioms.add(axiomAdapter.toOwlClassAssertionAxiom(axiom));
                break;
            case OBJECT_PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlObjectPropertyAssertionAxiom(axiom));
                break;
            case DATA_PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlDataPropertyAssertionAxiom(axiom));
                break;
            case ANNOTATION_PROPERTY:
                owlAxioms.add(axiomAdapter.toOwlAnnotationPropertyAssertionAxiom(axiom));
                break;
            default:
                owlAxioms.add(axiomAdapter.toOwlClassAssertionAxiom(axiom));
                owlAxioms.add(axiomAdapter.toOwlObjectPropertyAssertionAxiom(axiom));
                owlAxioms.add(axiomAdapter.toOwlDataPropertyAssertionAxiom(axiom));
                owlAxioms.add(axiomAdapter.toOwlAnnotationPropertyAssertionAxiom(axiom));
                break;
        }
        return owlAxioms;
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        startTransactionIfNotActive();
        return new MainAxiomLoader(this, ontologySnapshot).findAxioms(descriptor);
    }

    void persist(AxiomValueDescriptor descriptor) {
        startTransactionIfNotActive();
        new AxiomSaver(this, ontologySnapshot).persist(descriptor);
    }

    URI generateIdentifier(URI classUri) {
        startTransactionIfNotActive();
        return new IdentifierGenerator(ontology()).generateIdentifier(classUri);
    }

    void update(AxiomValueDescriptor descriptor) {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(this, ontologySnapshot).remove(descriptor);
        new AxiomSaver(this, ontologySnapshot).persist(descriptor);
    }

    void remove(AxiomDescriptor descriptor) {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(this, ontologySnapshot).remove(descriptor);
    }

    TypesHandler getTypesHandler() {
        startTransactionIfNotActive();
        return new TypesHandler(this, ontologySnapshot);
    }

    PropertiesHandler getPropertiesHandler() {
        startTransactionIfNotActive();
        return new PropertiesHandler(this, ontologySnapshot);
    }

    public void addTransactionalChanges(Collection<TransactionalChange> changes) {
        pendingChanges.removeIf(tc -> changes.stream().anyMatch(toAdd -> toAdd.overrides(tc)));
        pendingChanges.addAll(changes);
    }

    public ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler() {
        startTransactionIfNotActive();
        return ListHandler.getSimpleListHandler(this, ontologySnapshot);
    }

    public ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler() {
        startTransactionIfNotActive();
        return ListHandler.getReferencedListHandler(this, ontologySnapshot);
    }

    public OwlapiStatement createStatement(OwlapiConnection connection) {
        startTransactionIfNotActive();
        return new OwlapiStatement(statementExecutorFactory, connection);
    }

    public OwlapiPreparedStatement prepareStatement(String statement, OwlapiConnection connection) {
        startTransactionIfNotActive();
        return new OwlapiPreparedStatement(statementExecutorFactory, connection, statement);
    }

    public <T> T unwrap(Class<T> cls) throws OwlapiDriverException {
        startTransactionIfNotActive();
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        } else if (cls.isAssignableFrom(OWLOntology.class)) {
            return cls.cast(ontology());
        } else if (cls.isAssignableFrom(OWLReasoner.class)) {
            return cls.cast(reasoner());
        }
        throw new OwlapiDriverException("Unsupported type " + cls);
    }
}
