/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.sesame.config.Constants;
import cz.cvut.kbss.ontodriver.sesame.config.RuntimeConfiguration;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import cz.cvut.kbss.ontodriver.util.IdentifierUtils;
import cz.cvut.kbss.ontodriver.util.Transaction;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static cz.cvut.kbss.ontodriver.sesame.util.SesameUtils.toSesameIri;

class SesameAdapter implements Closeable, Wrapper {

    /**
     * Maximum number of attempts to generate a unique identifier
     */
    private static final int ID_GENERATION_THRESHOLD = 64;

    private final Connector connector;
    private final ValueFactory valueFactory;
    private final RuntimeConfiguration config;
    private final Transaction transaction;

    private boolean open = true;

    public SesameAdapter(Connector connector, RuntimeConfiguration config) {
        assert connector != null;

        this.connector = connector;
        this.valueFactory = connector.getValueFactory();
        this.config = config;
        this.transaction = new Transaction();
    }

    Connector getConnector() {
        return connector;
    }

    ValueFactory getValueFactory() {
        return valueFactory;
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

    void commit() throws SesameDriverException {
        if (transaction.isActive()) {
            transaction.commit();
            connector.commit();
            transaction.afterCommit();
        }
    }

    void rollback() throws SesameDriverException {
        if (transaction.isActive()) {
            transaction.rollback();
            connector.rollback();
            transaction.afterRollback();
        }
    }

    boolean isConsistent(URI context) {
        // Sesame currently doesn't support any consistency checking
        // functionality
        return true;
    }

    List<URI> getContexts() throws SesameDriverException {
        final List<Resource> contextIds = connector.getContexts();
        final List<URI> contexts = new ArrayList<>(contextIds.size());
        for (Resource res : contextIds) {
            final URI context = SesameUtils.toJavaUri(res);
            // We support only named contexts (no blank nodes)
            if (context != null) {
                contexts.add(context);
            }
        }
        return contexts;
    }

    URI generateIdentifier(URI classUri) throws SesameDriverException {
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

    private void startTransactionIfNotActive() throws SesameDriverException {
        if (!transaction.isActive()) {
            connector.begin();
            transaction.begin();
        }
    }

    private boolean isIdentifierUnique(URI identifier, URI classUri) throws SesameDriverException {
        return !connector.containsStatement(
                toSesameIri(identifier, valueFactory), RDF.TYPE,
                toSesameIri(classUri, valueFactory), true, Collections.emptySet());
    }

    boolean contains(Axiom<?> axiom, Set<URI> contexts) throws SesameDriverException {
        startTransactionIfNotActive();
        Value value;
        if (SesameUtils.isResourceIdentifier(axiom.getValue().getValue())) {
            value = valueFactory.createIRI(axiom.getValue().stringValue());
        } else {
            final String lang =
                    axiom.getAssertion().hasLanguage() ? axiom.getAssertion().getLanguage() : Constants.DEFAULT_LANG;
            value = SesameUtils.createLiteral(axiom.getValue().getValue(), lang, valueFactory);
        }
        return connector.containsStatement(
                toSesameIri(axiom.getSubject().getIdentifier(), valueFactory),
                toSesameIri(axiom.getAssertion().getIdentifier(), valueFactory), value,
                axiom.getAssertion().isInferred(),
                contexts.stream().map(c -> toSesameIri(c, valueFactory)).collect(Collectors.toSet()));

    }

    Collection<Axiom<?>> find(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        return new AxiomLoader(connector, config).loadAxioms(axiomDescriptor);
    }

    void persist(AxiomValueDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        new AxiomSaver(connector).persistAxioms(axiomDescriptor);
    }

    void update(AxiomValueDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(connector, valueFactory).remove(axiomDescriptor);
        new AxiomSaver(connector).persistAxioms(axiomDescriptor);
    }

    void remove(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(connector, valueFactory).remove(axiomDescriptor);
    }

    StatementExecutor getQueryExecutor() {
        return connector;
    }

    ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler() throws SesameDriverException {
        startTransactionIfNotActive();
        return ListHandler.createForSimpleList(connector, valueFactory);
    }

    ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler() throws
            SesameDriverException {
        startTransactionIfNotActive();
        return ListHandler.createForReferencedList(connector, valueFactory);
    }

    TypesHandler getTypesHandler() throws SesameDriverException {
        startTransactionIfNotActive();
        return new TypesHandler(connector, valueFactory);
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
