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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * This connector tracks transactional changes and writes them on commit to the {@link SharedStorageConnector}.
 */
public class ChangeTrackingStorageConnector extends AbstractStorageConnector {

    private final AbstractStorageConnector centralConnector;

    private final boolean useDefaultAsUnion;

    private boolean readOnly;

    private LocalModel localModel;

    ChangeTrackingStorageConnector(AbstractStorageConnector centralConnector) {
        super(centralConnector.configuration);
        this.centralConnector = centralConnector;
        this.useDefaultAsUnion =
                configuration != null && configuration.is(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION);
    }

    @Override
    public void begin() {
        transaction.begin();
        this.localModel = readOnly ? new DummyLocalModel() : new ChangeTrackingLocalModel(useDefaultAsUnion);
    }

    @Override
    public void commit() throws JenaDriverException {
        transaction.commit();
        try {
            centralConnector.begin();
            mergeRemovedStatements();
            mergeAddedStatements();
            centralConnector.commit();
            transaction.afterCommit();
        } catch (JenaDriverException e) {
            transaction.rollback();
            centralConnector.rollback();
            transaction.afterRollback();
            throw e;
        } finally {
            this.localModel = null;
        }
    }

    @Override
    public void setReadOnly(boolean readOnly) {
        if (transaction.isActive()) {
            throw new IllegalStateException("Cannot change read-only mode of an active transaction.");
        }
        this.readOnly = readOnly;
    }

    @Override
    public boolean isReadOnly() {
        return readOnly;
    }

    private void mergeRemovedStatements() {
        final Dataset removed = localModel.getRemoved();
        centralConnector.remove(removed.getDefaultModel().listStatements().toList(), null);
        removed.listNames().forEachRemaining(context -> {
            final Model model = removed.getNamedModel(context);
            centralConnector.remove(model.listStatements().toList(), context);
        });
        centralConnector.removePropertyValues(localModel.getRemovedSubjectPredicateStatements());
    }

    private void mergeAddedStatements() {
        final Dataset added = localModel.getAdded();
        centralConnector.add(added.getDefaultModel().listStatements().toList(), null);
        added.listNames().forEachRemaining(context -> {
            final Model model = added.getNamedModel(context);
            centralConnector.add(model.listStatements().toList(), context);
        });
    }

    @Override
    public void rollback() {
        transaction.rollback();
        this.localModel = null;
        transaction.afterRollback();
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        transaction.verifyActive();
        final Collection<Statement> existing = centralConnector.find(subject, property, value, contexts);
        return localModel.enhanceStatements(existing, subject, property, value, contexts);
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        transaction.verifyActive();
        final LocalModel.Containment localStatus = localModel.contains(subject, property, value, contexts);
        return localStatus == LocalModel.Containment.ADDED ||
                localStatus == LocalModel.Containment.UNKNOWN &&
                        centralConnector.contains(subject, property, value, contexts);
    }

    @Override
    public List<String> getContexts() {
        transaction.verifyActive();
        final List<String> centralContexts = centralConnector.getContexts();
        final Set<String> set = new LinkedHashSet<>(centralContexts);
        set.addAll(localModel.getContexts());
        return new ArrayList<>(set);
    }

    @Override
    public void add(List<Statement> statements, String context) {
        transaction.verifyActive();
        localModel.addStatements(statements, context);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        transaction.verifyActive();
        localModel.removeStatements(statements, context);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object, String context) {
        transaction.verifyActive();
        localModel.removeStatements(new ArrayList<>(find(subject, property, object, context != null ? Collections.singleton(context) : Collections.emptySet())), context);
    }

    @Override
    public void removePropertyValues(Collection<SubjectPredicateContext> spc) {
        transaction.verifyActive();
        localModel.removePropertyValues(spc);
    }

    @Override
    public AbstractResultSet executeSelectQuery(Query query, StatementOntology target) throws JenaDriverException {
        Objects.requireNonNull(query);
        // Since query results are not enhanced with transactional changes, do not require an active transaction
        return centralConnector.executeSelectQuery(query, target);
    }

    @Override
    public AbstractResultSet executeAskQuery(Query query, StatementOntology target) throws JenaDriverException {
        Objects.requireNonNull(query);
        // Since query results are not enhanced with transactional changes, do not require an active transaction
        return centralConnector.executeAskQuery(query, target);
    }

    @Override
    public void executeUpdate(String query, StatementOntology target) throws JenaDriverException {
        Objects.requireNonNull(query);
        // SPARQL Update queries have their own executor in Jena, so let them transcend the transactional boundaries
        centralConnector.executeUpdate(query, target);
    }

    @Override
    public synchronized void close() {
        if (transaction.isActive()) {
            rollback();
        }
        super.close();
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        return centralConnector.unwrap(cls);
    }
}
