package cz.cvut.kbss.ontodriver.sesame;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SesameAdapter implements Closeable {

	/** Maximum number of attempts to generate a unique identifier */
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
		this.language = getLanguage(properties);
		this.open = true;
		this.transaction = new Transaction();
	}

	private String getLanguage(Map<String, String> properties) {
		return properties.containsKey(OntoDriverProperties.ONTOLOGY_LANGUAGE) ? properties
				.get(OntoDriverProperties.ONTOLOGY_LANGUAGE) : null;
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

	Collection<Axiom<?>> find(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
		startTransactionIfNotActive();
		return new AxiomLoader(connector, valueFactory).loadAxioms(axiomDescriptor);
	}

	void persist(MutationAxiomDescriptor axiomDescriptor) throws SesameDriverException {
		startTransactionIfNotActive();
		if (individualExists(axiomDescriptor.getSubject(), axiomDescriptor.getSubjectContext())) {
			throw new OWLEntityExistsException("An individual with identifier "
					+ axiomDescriptor.getSubject() + " already exists in context "
					+ axiomDescriptor.getSubjectContext());
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

	void update(MutationAxiomDescriptor axiomDescriptor) throws SesameDriverException {
		startTransactionIfNotActive();
		// TODO
	}

	void remove(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
		startTransactionIfNotActive();
		// TODO
	}

	StatementExecutor getQueryExecutor() {
		return connector;
	}
}
