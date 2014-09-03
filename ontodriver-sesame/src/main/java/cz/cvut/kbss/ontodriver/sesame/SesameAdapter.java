package cz.cvut.kbss.ontodriver.sesame;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SesameAdapter implements Closeable {

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
		transaction.commit();
		connector.commit();
		transaction.afterCommit();
	}

	void rollback() throws SesameDriverException {
		transaction.rollback();
		connector.rollback();
		transaction.afterRollback();
	}

	boolean isConsistent(URI context) {
		// TODO Sesame currently doesn't support any consistency check function
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

	Collection<Axiom<?>> find(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
		startTransactionIfNotActive();
		return new AxiomLoader(connector, valueFactory).loadAxioms(axiomDescriptor);
	}

	void persist(MutationAxiomDescriptor axiomDescriptor) throws SesameDriverException {
		startTransactionIfNotActive();
		final List<Statement> statements = new ArrayList<>();
		for (Assertion assertion : axiomDescriptor.getAssertions()) {
			statements.addAll(createSesameStatements(axiomDescriptor.getSubject(), assertion,
					axiomDescriptor.getAssertionValues(assertion),
					axiomDescriptor.getAssertionContext(assertion)));
		}
		connector.addStatements(statements);
	}

	private void startTransactionIfNotActive() throws SesameDriverException {
		if (!transaction.isActive()) {
			connector.begin();
			transaction.begin();
		}
	}

	private Collection<? extends Statement> createSesameStatements(NamedResource subject,
			Assertion assertion, List<Value<?>> assertionValues, URI assertionContext)
			throws SesameDriverException {
		final List<Statement> statements = new ArrayList<>(assertionValues.size());

		final org.openrdf.model.Resource subjectUri = SesameUtils.toSesameUri(
				subject.getIdentifier(), valueFactory);
		final org.openrdf.model.URI property = SesameUtils.toSesameUri(assertion.getIdentifier(),
				valueFactory);
		final org.openrdf.model.URI context = assertionContext != null ? SesameUtils.toSesameUri(
				assertionContext, valueFactory) : null;
		for (Value<?> val : assertionValues) {
			org.openrdf.model.Value value = toSesameValue(assertion, val);
			statements.add(createStatement(subjectUri, property, value, context));
		}
		return statements;
	}

	private org.openrdf.model.Value toSesameValue(Assertion assertion, Value<?> val)
			throws SesameDriverException {
		switch (assertion.getType()) {
		case ANNOTATION_PROPERTY:
		case DATA_PROPERTY:
			return SesameUtils.createDataPropertyLiteral(val.getValue(), language, valueFactory);
		case CLASS:
		case OBJECT_PROPERTY:
			return getValueAsSesameUri(val);
		case PROPERTY:
			return resolvePropertyValue(assertion, val);
		default:
			// Failsafe
			throw new IllegalArgumentException("Unsupported assertion type " + assertion.getType());
		}
	}

	private org.openrdf.model.URI getValueAsSesameUri(Value<?> val) throws SesameDriverException {
		try {
			return valueFactory.createURI(val.getValue().toString());
		} catch (IllegalArgumentException e) {
			throw new SesameDriverException(e);
		}
	}

	private org.openrdf.model.Value resolvePropertyValue(Assertion assertion, Value<?> val) {
		try {
			return getValueAsSesameUri(val);
		} catch (SesameDriverException e) {
			return SesameUtils.createDataPropertyLiteral(val.getValue(), language, valueFactory);
		}
	}

	private Statement createStatement(Resource subject, org.openrdf.model.URI property,
			org.openrdf.model.Value value, org.openrdf.model.URI context) {
		if (context != null) {
			return valueFactory.createStatement(subject, property, value, context);
		} else {
			return valueFactory.createStatement(subject, property, value);
		}
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
