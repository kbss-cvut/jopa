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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.container.ContainerHandler;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.list.ReferencedListHandler;
import cz.cvut.kbss.ontodriver.jena.list.SimpleListHandler;
import cz.cvut.kbss.ontodriver.jena.query.JenaPreparedStatement;
import cz.cvut.kbss.ontodriver.jena.query.JenaStatement;
import cz.cvut.kbss.ontodriver.jena.util.IdentifierGenerator;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.util.Transaction;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Transformations between OntoDriver API-based values and Jena-based ones.
 * <p>
 * Implementation notes:
 * <ul>
 * <li>Datatype literal types are based on Jena's mapping, as described in a table at <a
 * href="https://jena.apache.org/documentation/notes/typed-literals.html">https://jena.apache.org/documentation/notes/typed-literals.html</a></li>
 * </ul>
 */
public class JenaAdapter implements Wrapper {

    private final Transaction transaction = new Transaction();

    private final StorageConnector connector;
    private final InferredStorageConnector inferenceConnector;

    JenaAdapter(StorageConnector connector, InferredStorageConnector inferenceConnector) {
        this.connector = connector;
        this.inferenceConnector = inferenceConnector;
    }

    void commit() throws JenaDriverException {
        if (transaction.isActive()) {
            transaction.commit();
            connector.commit();
            transaction.afterCommit();
        }
    }

    void rollback() {
        if (transaction.isActive()) {
            transaction.rollback();
            connector.rollback();
            transaction.afterRollback();
        }
    }

    void persist(AxiomValueDescriptor descriptor) {
        beginTransactionIfNotActive();
        new AxiomSaver(connector).saveAxioms(descriptor);
    }

    private void beginTransactionIfNotActive() {
        if (!transaction.isActive()) {
            connector.begin();
            transaction.begin();
        }
    }

    Collection<Axiom<?>> find(AxiomDescriptor descriptor) {
        beginTransactionIfNotActive();
        return new MainAxiomLoader(connector, inferenceConnector).find(descriptor);
    }

    boolean contains(Axiom<?> axiom, Set<URI> contexts) {
        beginTransactionIfNotActive();
        return new MainAxiomLoader(connector, inferenceConnector).contains(axiom, contexts);
    }

    boolean isInferred(Axiom<?> axiom, Set<URI> contexts) {
        beginTransactionIfNotActive();
        return new MainAxiomLoader(connector, inferenceConnector).isInferred(axiom, contexts);
    }

    List<URI> getContext() {
        beginTransactionIfNotActive();
        return connector.getContexts().stream().map(URI::create).collect(Collectors.toList());
    }

    URI generateIdentifier(URI classUri) {
        beginTransactionIfNotActive();
        return new IdentifierGenerator(connector).generateIdentifier(classUri);
    }

    boolean isConsistent(URI context) {
        beginTransactionIfNotActive();
        return inferenceConnector.isConsistent(context != null ? context.toString() : null);
    }

    void update(AxiomValueDescriptor descriptor) {
        beginTransactionIfNotActive();
        new EpistemicAxiomRemover(connector).remove(descriptor);
        new AxiomSaver(connector).saveAxioms(descriptor);
    }

    void remove(AxiomDescriptor descriptor) {
        beginTransactionIfNotActive();
        new EpistemicAxiomRemover(connector).remove(descriptor);
    }

    TypesHandler typesHandler() {
        beginTransactionIfNotActive();
        return new TypesHandler(connector, inferenceConnector);
    }

    PropertiesHandler propertiesHandler() {
        beginTransactionIfNotActive();
        return new PropertiesHandler(connector);
    }

    public SimpleListHandler simpleListHandler() {
        beginTransactionIfNotActive();
        return new SimpleListHandler(connector);
    }

    public ReferencedListHandler referencedListHandler() {
        beginTransactionIfNotActive();
        return new ReferencedListHandler(connector);
    }

    public ContainerHandler containerHandler() {
        beginTransactionIfNotActive();
        return new ContainerHandler(connector);
    }

    JenaStatement createStatement() {
        beginTransactionIfNotActive();
        return new JenaStatement(inferenceConnector);
    }

    JenaPreparedStatement prepareStatement(String sparql) {
        beginTransactionIfNotActive();
        return new JenaPreparedStatement(inferenceConnector, sparql);
    }

    void close() throws JenaDriverException {
        connector.close();
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return connector.unwrap(cls);
    }
}
