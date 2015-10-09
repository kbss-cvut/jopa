package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.descriptors.*;
import cz.cvut.kbss.ontodriver_new.exception.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver_new.exception.OWLIndividualExistsException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import java.net.URI;
import java.util.*;

class SesameAdapter implements Closeable {

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

    public SesameAdapter(Connector connector, Map<String, String> properties) {
        assert connector != null;

        this.connector = connector;
        this.valueFactory = connector.getValueFactory();
        this.language = resolveLanguage(properties);
        this.open = true;
        this.transaction = new Transaction();
    }

    private String resolveLanguage(Map<String, String> properties) {
        return properties.containsKey(OntoDriverProperties.ONTOLOGY_LANGUAGE) ? properties
                .get(OntoDriverProperties.ONTOLOGY_LANGUAGE) : null;
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
        return !(connector.findStatements(
                SesameUtils.toSesameUri(axiom.getSubject().getIdentifier(), valueFactory),
                SesameUtils.toSesameUri(axiom.getAssertion().getIdentifier(), valueFactory), value,
                true, SesameUtils.toSesameUri(context, valueFactory)).isEmpty());
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
        final Collection<Statement> result = connector.findStatements(sesameSubject, RDF.TYPE,
                null, true, SesameUtils.toSesameUri(subjectContext, valueFactory));
        return !result.isEmpty();
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
}
