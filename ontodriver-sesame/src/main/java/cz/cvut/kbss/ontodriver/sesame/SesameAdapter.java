/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Wrapper;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.*;
import cz.cvut.kbss.ontodriver.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.exception.OWLIndividualExistsException;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;

class SesameAdapter implements Closeable, Wrapper {

    /**
     * Maximum number of attempts to generate a unique identifier
     */
    private static final int ID_GENERATION_THRESHOLD = 64;
    private static final Random RANDOM = new Random();

    private final Connector connector;
    private final ValueFactory valueFactory;
    private final String language;
    private boolean open;
    private final Transaction transaction;

    public SesameAdapter(Connector connector, Configuration configuration) {
        assert connector != null;

        this.connector = connector;
        this.valueFactory = connector.getValueFactory();
        this.language = configuration.getProperty(ConfigParam.ONTOLOGY_LANGUAGE);
        this.open = true;
        this.transaction = new Transaction();
    }

    Connector getConnector() {
        return connector;
    }

    ValueFactory getValueFactory() {
        return valueFactory;
    }

    String getLanguage() {
        return language;
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
            if (classUri.getFragment() != null) {
                id = URI.create(classUri.toString() + "_instance" + RANDOM.nextInt());
            } else {
                String base = classUri.toString();
                if (base.endsWith("/")) {
                    id = URI.create(base + "_instance" + RANDOM.nextInt());
                } else {
                    id = URI.create(base + "#instance" + RANDOM.nextInt());
                }
            }
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
        final Collection<Statement> stmts = connector.findStatements(
                SesameUtils.toSesameUri(identifier, valueFactory), RDF.TYPE,
                SesameUtils.toSesameUri(classUri, valueFactory), true);
        return stmts.isEmpty();
    }

    boolean contains(Axiom<?> axiom, URI context) throws SesameDriverException {
        startTransactionIfNotActive();
        Value value;
        if (SesameUtils.isResourceIdentifier(axiom.getValue().getValue())) {
            value = valueFactory.createURI(axiom.getValue().stringValue());
        } else {
            value = SesameUtils.createDataPropertyLiteral(axiom.getValue().getValue(), language,
                    valueFactory);
        }
        final org.openrdf.model.URI sesameContext = SesameUtils.toSesameUri(context, valueFactory);
        return !findStatements(
                SesameUtils.toSesameUri(axiom.getSubject().getIdentifier(), valueFactory),
                SesameUtils.toSesameUri(axiom.getAssertion().getIdentifier(), valueFactory), value,
                axiom.getAssertion().isInferred(), sesameContext).isEmpty();
    }

    private Collection<Statement> findStatements(Resource subject, org.openrdf.model.URI property, Value value,
                                                 boolean includeInferred, org.openrdf.model.URI context)
            throws SesameDriverException {
        if (context != null) {
            return connector.findStatements(subject, property, value, includeInferred, context);
        } else {
            return connector.findStatements(subject, property, value, includeInferred);
        }
    }

    Collection<Axiom<?>> find(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        return new AxiomLoader(connector, valueFactory).loadAxioms(axiomDescriptor);
    }

    void persist(AxiomValueDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        if (individualExists(axiomDescriptor.getSubject(), axiomDescriptor.getSubjectContext())) {
            throw new SesameDriverException(new OWLIndividualExistsException("An individual with identifier "
                    + axiomDescriptor.getSubject() + " already exists in context "
                    + axiomDescriptor.getSubjectContext()));
        }
        new AxiomSaver(connector, valueFactory, language).persistAxioms(axiomDescriptor);
    }

    private boolean individualExists(NamedResource subject, URI subjectContext)
            throws SesameDriverException {
        final org.openrdf.model.URI sesameSubject = SesameUtils.toSesameUri(
                subject.getIdentifier(), valueFactory);
        return !findStatements(sesameSubject, RDF.TYPE,
                null, true, SesameUtils.toSesameUri(subjectContext, valueFactory)).isEmpty();
    }

    void update(AxiomValueDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(connector, valueFactory, language).remove(axiomDescriptor);
        new AxiomSaver(connector, valueFactory, language).persistAxioms(axiomDescriptor);
    }

    void remove(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
        startTransactionIfNotActive();
        new EpistemicAxiomRemover(connector, valueFactory, language).remove(axiomDescriptor);
    }

    StatementExecutor getQueryExecutor() {
        return connector;
    }

    ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> getSimpleListHandler() {
        return ListHandler.createForSimpleList(connector, valueFactory);
    }

    ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> getReferencedListHandler() {
        return ListHandler.createForReferencedList(connector, valueFactory);
    }

    TypesHandler getTypesHandler() {
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
