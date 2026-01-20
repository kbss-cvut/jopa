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
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.RepositoryMetadata;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.rdf4j.config.Constants;
import cz.cvut.kbss.ontodriver.rdf4j.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.rdf4j.container.ContainerHandler;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.list.ReferencedListHandler;
import cz.cvut.kbss.ontodriver.rdf4j.list.SimpleListHandler;
import cz.cvut.kbss.ontodriver.rdf4j.util.Rdf4jUtils;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import cz.cvut.kbss.ontodriver.util.Transaction;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Rdf4jAdapter implements Closeable, Wrapper {

    /**
     * Maximum number of attempts to generate a unique identifier
     */
    private static final int ID_GENERATION_THRESHOLD = 64;

    private final RepoConnection connector;
    private final ValueFactory valueFactory;
    private final RuntimeConfiguration config;
    private final Transaction transaction;

    private boolean open = true;

    public Rdf4jAdapter(RepoConnection connector, RuntimeConfiguration config) {
        assert connector != null;

        this.connector = connector;
        this.valueFactory = connector.getValueFactory();
        this.config = config;
        this.transaction = new Transaction();
    }

    RepoConnection getConnector() {
        return connector;
    }

    RuntimeConfiguration getConfig() {
        return config;
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            connector.close();
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void commit() throws Rdf4jDriverException {
        if (transaction.isActive()) {
            transaction.commit();
            connector.commit();
            transaction.afterCommit();
        }
    }

    void rollback() throws Rdf4jDriverException {
        if (transaction.isActive()) {
            transaction.rollback();
            connector.rollback();
            transaction.afterRollback();
        }
    }

    void setReadOnly(boolean readOnly) {
        connector.setReadOnly(readOnly);
    }

    boolean isReadOnly() {
        return connector.isReadOnly();
    }

    boolean isConsistent(URI context) {
        // RDF4J currently doesn't support any consistency checking functionality
        return true;
    }

    List<URI> getContexts() throws Rdf4jDriverException {
        startTransactionIfNotActive();
        final List<Resource> contextIds = connector.getContexts();
        final List<URI> contexts = new ArrayList<>(contextIds.size());
        for (Resource res : contextIds) {
            final URI context = Rdf4jUtils.toJavaUri(res);
            // We support only named contexts (no blank nodes)
            if (context != null) {
                contexts.add(context);
            }
        }
        return contexts;
    }

    URI generateIdentifier(URI classUri) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        boolean unique = false;
        URI id = null;
        int counter = 0;
        while (!unique && counter++ < ID_GENERATION_THRESHOLD) {
            id = IdentifierUtils.generateIdentifier(classUri);
            unique = isIdentifierUnique(id, classUri);
        }
        if (!unique) {
            throw new IdentifierGenerationException("Unable to generate a unique identifier.");
        }
        return id;

    }

    private void startTransactionIfNotActive() throws Rdf4jDriverException {
        if (!transaction.isActive()) {
            connector.begin();
            transaction.begin();
        }
    }

    private boolean isIdentifierUnique(URI identifier, URI classUri) throws Rdf4jDriverException {
        return !connector.containsStatement(
                Rdf4jUtils.toRdf4jIri(identifier, valueFactory), RDF.TYPE,
                Rdf4jUtils.toRdf4jIri(classUri, valueFactory), true, Collections.emptySet());
    }

    boolean contains(Axiom<?> axiom, Set<URI> contexts) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        final Value value = axiomObjectToRdf4jValue(axiom);
        return connector.containsStatement(
                Rdf4jUtils.toRdf4jIri(axiom.getSubject(), valueFactory),
                Rdf4jUtils.toRdf4jIri(axiom.getAssertion(), valueFactory), value,
                axiom.getAssertion().isInferred(),
                contexts.stream().map(c -> Rdf4jUtils.toRdf4jIri(c, valueFactory)).collect(Collectors.toSet()));

    }

    private Value axiomObjectToRdf4jValue(Axiom<?> axiom) {
        final Value value;
        if (Rdf4jUtils.isResourceIdentifier(axiom.getValue().getValue())) {
            value = valueFactory.createIRI(axiom.getValue().stringValue());
        } else {
            final String lang =
                    axiom.getAssertion().hasLanguage() ? axiom.getAssertion().getLanguage() : Constants.DEFAULT_LANG;
            value = Rdf4jUtils.createLiteral(axiom.getValue().getValue(), lang, valueFactory);
        }
        return value;
    }

    boolean isInferred(Axiom<?> axiom, Set<URI> contexts) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        final Value value = axiomObjectToRdf4jValue(axiom);
        final Statement s = valueFactory.createStatement(Rdf4jUtils.toRdf4jIri(axiom.getSubject(), valueFactory),
                Rdf4jUtils.toRdf4jIri(axiom.getAssertion(), valueFactory), value);
        return connector.isInferred(s, contexts.stream().map(c -> Rdf4jUtils.toRdf4jIri(c, valueFactory))
                                               .collect(Collectors.toSet()));
    }

    Collection<Axiom<?>> find(AxiomDescriptor axiomDescriptor) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        return new AxiomLoader(connector, config).loadAxioms(axiomDescriptor);
    }

    void persist(AxiomValueDescriptor axiomDescriptor) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        new AxiomSaver(connector).persistAxioms(axiomDescriptor);
    }

    void update(AxiomValueDescriptor axiomDescriptor) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(connector, valueFactory).remove(axiomDescriptor);
        new AxiomSaver(connector).persistAxioms(axiomDescriptor);
    }

    void remove(AxiomDescriptor axiomDescriptor) throws Rdf4jDriverException {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(connector, valueFactory).remove(axiomDescriptor);
    }

    StatementExecutor getQueryExecutor() {
        return connector;
    }

    SimpleListHandler getSimpleListHandler() throws Rdf4jDriverException {
        startTransactionIfNotActive();
        return new SimpleListHandler(connector, valueFactory);
    }

    ReferencedListHandler getReferencedListHandler() throws
            Rdf4jDriverException {
        startTransactionIfNotActive();
        return new ReferencedListHandler(connector, valueFactory);
    }

    TypesHandler getTypesHandler() throws Rdf4jDriverException {
        startTransactionIfNotActive();
        return new TypesHandler(connector, valueFactory);
    }

    ContainerHandler getContainerHandler() throws Rdf4jDriverException {
        startTransactionIfNotActive();
        return new ContainerHandler(connector, valueFactory);
    }

    public RepositoryMetadata getRepositoryMetadata() {
        return new Rdf4jRepositoryMetadata(connector.getProductName());
    }

    @Override
    public <T> T unwrap(Class<T> cls) throws OntoDriverException {
        if (cls.isAssignableFrom(this.getClass())) {
            return cls.cast(this);
        } else if (cls.isAssignableFrom(valueFactory.getClass())) {
            return cls.cast(valueFactory);
        }
        return connector.unwrap(cls);
    }
}
