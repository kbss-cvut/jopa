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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import cz.cvut.kbss.ontodriver.jena.query.AskResultSet;
import cz.cvut.kbss.ontodriver.jena.query.SelectResultSet;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.system.Txn;
import org.apache.jena.update.UpdateAction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Main storage connector using the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#READ_COMMITTED}
 * connector strategy.
 * <p>
 * Adding statements to it actually adds them to the repository.
 * <p>
 * Note on transactions:
 * <p>
 * Starting a transaction on this connector also starts a write transaction on the underlying dataset. Commit then
 * commits the transaction. Therefore, these transactions should be short. Reading can happen in parallel (as per Jena
 * documentation).
 */
public class SharedStorageConnector extends AbstractStorageConnector {

    SharedStorageConnector(DriverConfiguration configuration) {
        super(configuration);
    }

    @Override
    void initialize() {
        this.storage = Storage.create(configuration);
    }

    @Override
    public synchronized void begin() {
        ensureOpen();
        transaction.begin();
        storage.begin(ReadWrite.WRITE);
    }

    @Override
    public synchronized void commit() throws JenaDriverException {
        ensureTransactionalState();
        transaction.commit();
        storage.writeChanges();
        storage.commit();
        transaction.afterCommit();
    }

    void ensureTransactionalState() {
        ensureOpen();
        transaction.verifyActive();
    }

    @Override
    public void rollback() {
        ensureOpen();
        transaction.rollback();
        storage.rollback();
        transaction.afterRollback();
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        ensureOpen();
        return Txn.calculateRead(storage.getTransactional(), () -> {
            final List<Statement> result;
            if (contexts.isEmpty()) {
                result = storage.getDefaultGraph().listStatements(subject, property, value).toList();
            } else {
                result = contexts.stream()
                                 .map(c -> storage.getNamedGraph(c).listStatements(subject, property, value).toList())
                                 .flatMap(Collection::stream).collect(Collectors.toList());
            }
            return result;
        });
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        ensureOpen();
        return Txn.calculateRead(storage.getTransactional(), () -> {
            if (contexts.isEmpty()) {
                return storage.getDefaultGraph().contains(subject, property, value);
            } else {
                return contexts.stream().anyMatch(c -> storage.getNamedGraph(c).contains(subject, property, value));
            }
        });
    }

    @Override
    public List<String> getContexts() {
        ensureOpen();
        final Iterator<String> it = Txn
                .calculateRead(storage.getTransactional(), () -> storage.getDataset().listNames());
        final List<String> contexts = new ArrayList<>();
        it.forEachRemaining(contexts::add);
        return contexts;
    }

    @Override
    public void add(List<Statement> statements, String context) {
        ensureTransactionalState();
        storage.add(statements, context);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        ensureTransactionalState();
        storage.remove(statements, context);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object, String context) {
        ensureTransactionalState();
        if (context != null) {
            storage.remove(storage.getNamedGraph(context).listStatements(subject, property, object), context);
        } else {
            storage.remove(storage.getDefaultGraph().listStatements(subject, property, object), null);
        }
    }

    @Override
    public void removePropertyValues(Collection<SubjectPredicateContext> spc) {
        ensureTransactionalState();
        spc.forEach(s -> {
            if (s.getContexts().isEmpty()) {
                storage.remove(storage.getDefaultGraph()
                                      .listStatements(s.getSubject(), s.getPredicate(), (RDFNode) null), null);
            } else {
                s.getContexts().forEach(c -> storage.remove(storage.getNamedGraph(c)
                                                                   .listStatements(s.getSubject(), s.getPredicate(), (RDFNode) null), c));
            }
        });
    }

    @Override
    public AbstractResultSet executeSelectQuery(Query query, StatementOntology target) throws JenaDriverException {
        ensureOpen();
        try {
            QueryExecution exec = storage.prepareQuery(query);
            final org.apache.jena.query.ResultSet rs = exec.execSelect();
            // The QueryExecution is closed by the SelectResultSet (so that it has access to the results)
            return new SelectResultSet(exec, rs);
        } catch (RuntimeException e) {
            throw queryFailed(query, e);
        }
    }

    private static JenaDriverException queryFailed(Object query, RuntimeException e) {
        return new JenaDriverException("Execution of query " + query + " failed.", e);
    }

    @Override
    public AbstractResultSet executeAskQuery(Query query, StatementOntology target) throws JenaDriverException {
        ensureOpen();
        try (final QueryExecution exec = storage.prepareQuery(query)) {
            return new AskResultSet(exec.execAsk());
        } catch (RuntimeException e) {
            throw queryFailed(query, e);
        }
    }

    @Override
    public void executeUpdate(String query, StatementOntology target) throws JenaDriverException {
        ensureOpen();
        try {
            UpdateAction.parseExecute(query, storage.getDataset());
        } catch (RuntimeException e) {
            throw queryFailed(query, e);
        }
    }

    @Override
    public synchronized void close() {
        if (!isOpen()) {
            return;
        }
        if (storage != null) {
            storage.close();
        }
        super.close();
    }

    /**
     * Reloads data from the underlying storage (if possible).
     * <p>
     * Note that this applies only to RDF file-based storage access, other storage do not support reloading.
     */
    public synchronized void reloadStorage() {
        ensureOpen();
        storage.reload();
    }

    /**
     * Sets new dataset on the underlying storage.
     * <p>
     * Note that this is supported only for in-memory storage.
     *
     * @param dataset The dataset to use
     */
    public synchronized void setDataset(Dataset dataset) {
        ensureOpen();
        storage.setDataset(dataset);
    }
}
