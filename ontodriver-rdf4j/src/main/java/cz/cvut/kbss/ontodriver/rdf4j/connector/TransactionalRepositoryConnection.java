/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector;


import org.eclipse.rdf4j.common.iteration.Iteration;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.query.*;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryResult;
import org.eclipse.rdf4j.rio.ParserConfig;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFHandler;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.net.URL;

/**
 * Wraps a standard RFD4J {@link RepositoryConnection} and prevents its closing.
 * <p>
 * This is because the connector will handle closing when a transaction finishes or the connector is closed.
 *
 * @see PoolingStorageConnector
 */
class TransactionalRepositoryConnection implements RepositoryConnection {

    private final RepositoryConnection wrappedConnection;

    TransactionalRepositoryConnection(RepositoryConnection wrappedConnection) {
        this.wrappedConnection = wrappedConnection;
    }

    @Override
    public Repository getRepository() {
        return wrappedConnection.getRepository();
    }

    @Override
    public void setParserConfig(ParserConfig config) {
        wrappedConnection.setParserConfig(config);
    }

    @Override
    public ParserConfig getParserConfig() {
        return wrappedConnection.getParserConfig();
    }

    @Override
    public ValueFactory getValueFactory() {
        return wrappedConnection.getValueFactory();
    }

    @Override
    public boolean isOpen() {
        return wrappedConnection.isOpen();
    }

    @Override
    public void close() {
        // Do nothing !!!
    }

    @Override
    public Query prepareQuery(QueryLanguage ql, String query) {
        return wrappedConnection.prepareQuery(ql, query);
    }

    @Override
    public Query prepareQuery(QueryLanguage ql, String query, String baseURI) {
        return wrappedConnection.prepareQuery(ql, query, baseURI);
    }

    @Override
    public TupleQuery prepareTupleQuery(QueryLanguage ql, String query) {
        return wrappedConnection.prepareTupleQuery(ql, query);
    }

    @Override
    public TupleQuery prepareTupleQuery(QueryLanguage ql, String query, String baseURI) {
        return wrappedConnection.prepareTupleQuery(ql, query, baseURI);
    }

    @Override
    public GraphQuery prepareGraphQuery(QueryLanguage ql, String query) {
        return wrappedConnection.prepareGraphQuery(ql, query);
    }

    @Override
    public GraphQuery prepareGraphQuery(QueryLanguage ql, String query, String baseURI) {
        return wrappedConnection.prepareGraphQuery(ql, query, baseURI);
    }

    @Override
    public BooleanQuery prepareBooleanQuery(QueryLanguage ql, String query) {
        return wrappedConnection.prepareBooleanQuery(ql, query);
    }

    @Override
    public BooleanQuery prepareBooleanQuery(QueryLanguage ql, String query, String baseURI) {
        return wrappedConnection.prepareBooleanQuery(ql, query, baseURI);
    }

    @Override
    public Update prepareUpdate(QueryLanguage ql, String update) {
        return wrappedConnection.prepareUpdate(ql, update);
    }

    @Override
    public Update prepareUpdate(QueryLanguage ql, String update, String baseURI) {
        return wrappedConnection.prepareUpdate(ql, update, baseURI);
    }

    @Override
    public RepositoryResult<Resource> getContextIDs() {
        return wrappedConnection.getContextIDs();
    }

    @Override
    public RepositoryResult<Statement> getStatements(Resource subj, IRI pred, Value obj, boolean includeInferred,
                                                     Resource... contexts) {
        return wrappedConnection.getStatements(subj, pred, obj, includeInferred, contexts);
    }

    @Override
    public boolean hasStatement(Resource subj, IRI pred, Value obj, boolean includeInferred, Resource... contexts) {
        return wrappedConnection.hasStatement(subj, pred, obj, includeInferred, contexts);
    }

    @Override
    public boolean hasStatement(Statement st, boolean includeInferred, Resource... contexts) {
        return wrappedConnection.hasStatement(st, includeInferred, contexts);
    }

    @Override
    public void exportStatements(Resource subj, IRI pred, Value obj, boolean includeInferred, RDFHandler handler,
                                 Resource... contexts) {
        wrappedConnection.exportStatements(subj, pred, obj, includeInferred, handler, contexts);
    }

    @Override
    public void export(RDFHandler handler, Resource... contexts) {
        wrappedConnection.export(handler, contexts);
    }

    @Override
    public long size(Resource... contexts) {
        return wrappedConnection.size(contexts);
    }

    @Override
    public boolean isEmpty() {
        return wrappedConnection.isEmpty();
    }

    @Deprecated
    @Override
    public void setAutoCommit(boolean autoCommit) {
        wrappedConnection.setAutoCommit(autoCommit);
    }

    @Deprecated
    @Override
    public boolean isAutoCommit() {
        return wrappedConnection.isAutoCommit();
    }

    @Override
    public boolean isActive() {
        return wrappedConnection.isActive();
    }

    @Override
    public void setIsolationLevel(IsolationLevel level) {
        wrappedConnection.setIsolationLevel(level);
    }

    @Override
    public IsolationLevel getIsolationLevel() {
        return wrappedConnection.getIsolationLevel();
    }

    @Override
    public void begin() {
        wrappedConnection.begin();
    }

    @Override
    public void begin(IsolationLevel level) {
        wrappedConnection.begin(level);
    }

    @Override
    public void commit() {
        wrappedConnection.commit();
    }

    @Override
    public void rollback() {
        wrappedConnection.rollback();
    }

    @Override
    public void add(InputStream in, String baseURI, RDFFormat dataFormat, Resource... contexts)
            throws IOException {
        wrappedConnection.add(in, baseURI, dataFormat, contexts);
    }

    @Override
    public void add(Reader reader, String baseURI, RDFFormat dataFormat, Resource... contexts)
            throws IOException {
        wrappedConnection.add(reader, baseURI, dataFormat, contexts);
    }

    @Override
    public void add(URL url, String baseURI, RDFFormat dataFormat, Resource... contexts)
            throws IOException {
        wrappedConnection.add(url, baseURI, dataFormat, contexts);
    }

    @Override
    public void add(File file, String baseURI, RDFFormat dataFormat, Resource... contexts)
            throws IOException {
        wrappedConnection.add(file, baseURI, dataFormat, contexts);
    }

    @Override
    public void add(Resource subject, IRI predicate, Value object, Resource... contexts) {
        wrappedConnection.add(subject, predicate, object, contexts);
    }

    @Override
    public void add(Statement st, Resource... contexts) {
        wrappedConnection.add(st, contexts);
    }

    @Override
    public void add(Iterable<? extends Statement> statements, Resource... contexts) {
        wrappedConnection.add(statements, contexts);
    }

    @Override
    public <E extends Exception> void add(Iteration<? extends Statement, E> statements, Resource... contexts) throws E {
        wrappedConnection.add(statements, contexts);
    }

    @Override
    public void remove(Resource subject, IRI predicate, Value object, Resource... contexts) {
        wrappedConnection.remove(subject, predicate, object, contexts);
    }

    @Override
    public void remove(Statement st, Resource... contexts) {
        wrappedConnection.remove(st, contexts);
    }

    @Override
    public void remove(Iterable<? extends Statement> statements, Resource... contexts) {
        wrappedConnection.remove(statements, contexts);
    }

    @Override
    public <E extends Exception> void remove(Iteration<? extends Statement, E> statements, Resource... contexts)
            throws E {
        wrappedConnection.remove(statements, contexts);
    }

    @Override
    public void clear(Resource... contexts) {
        wrappedConnection.clear(contexts);
    }

    @Override
    public RepositoryResult<Namespace> getNamespaces() {
        return wrappedConnection.getNamespaces();
    }

    @Override
    public String getNamespace(String prefix) {
        return wrappedConnection.getNamespace(prefix);
    }

    @Override
    public void setNamespace(String prefix, String name) {
        wrappedConnection.setNamespace(prefix, name);
    }

    @Override
    public void removeNamespace(String prefix) {
        wrappedConnection.removeNamespace(prefix);
    }

    @Override
    public void clearNamespaces() {
        wrappedConnection.clearNamespaces();
    }
}
