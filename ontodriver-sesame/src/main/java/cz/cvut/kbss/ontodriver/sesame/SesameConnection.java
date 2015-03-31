package cz.cvut.kbss.ontodriver.sesame;

import static cz.cvut.kbss.jopa.utils.ErrorUtils.constructNPXMessage;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.IdentifierGenerationException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.Lists;
import cz.cvut.kbss.ontodriver_new.Properties;
import cz.cvut.kbss.ontodriver_new.Types;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class SesameConnection implements Connection {

	private SesameAdapter adapter;
	private boolean open;
	private boolean autoCommit;

    // TODO Remove coupling between lists, types and properties and connection by introducing callbacks
	private Lists lists;
	private Types types;
    private Properties properties;

	private final Set<ConnectionListener> listeners;

	public SesameConnection(SesameAdapter adapter) {
		assert adapter != null;
		this.adapter = adapter;
		this.listeners = new HashSet<>(4);
		this.open = true;
	}

	void setLists(SesameLists lists) {
		this.lists = lists;
	}

	void setTypes(SesameTypes types) {
		this.types = types;
	}

    public void setProperties(Properties properties) {
        this.properties = properties;
    }

    void addListener(ConnectionListener listener) {
		assert listener != null;
		listeners.add(listener);
	}

	void removeListener(ConnectionListener listener) {
		assert listener != null;
		listeners.remove(listener);
	}

	@Override
	public void close() throws Exception {
		if (!open) {
			return;
		}
		try {
			adapter.close();
			for (ConnectionListener listener : listeners) {
				listener.connectionClosed(this);
			}
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public void commit() throws OntoDriverException {
		ensureOpen();
		if (autoCommit) {
			return;
		}
		adapter.commit();
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		if (autoCommit) {
			return;
		}
		adapter.rollback();
	}

	@Override
	public void setAutoCommit(boolean autoCommit) {
		ensureOpen();
		this.autoCommit = autoCommit;
	}

	@Override
	public boolean isAutoCommit() {
		ensureOpen();
		return autoCommit;
	}

	@Override
	public Statement createStatement() throws OntoDriverException {
		ensureOpen();
		return new SesameStatement(adapter.getQueryExecutor());
	}

	@Override
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(sparql, constructNPXMessage("sparql"));
		if (sparql.isEmpty()) {
			throw new IllegalArgumentException("The value for prepared statement cannot be empty.");
		}
		return new SesamePreparedStatement(adapter.getQueryExecutor(), sparql);
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		ensureOpen();
		return adapter.isConsistent(context);
	}

	@Override
	public List<URI> getContexts() throws OntoDriverException {
		ensureOpen();
		return adapter.getContexts();
	}

	@Override
	public URI generateIdentifier(URI classUri) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(classUri, constructNPXMessage("classUri"));
		try {
			return adapter.generateIdentifier(classUri);
		} catch (IdentifierGenerationException e) {
			throw new SesameDriverException(e);
		}
	}

	@Override
	public boolean contains(Axiom<?> axiom, URI context) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(axiom, constructNPXMessage("axiom"));
		return adapter.contains(axiom, context);
	}

	@Override
	public Collection<Axiom<?>> find(AxiomDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		return adapter.find(descriptor);
	}

	@Override
	public void persist(AxiomValueDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		adapter.persist(descriptor);
		commitIfAuto();
	}

	@Override
	public void update(AxiomValueDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		adapter.update(descriptor);
		commitIfAuto();
	}

	@Override
	public void remove(AxiomDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage("descriptor"));
		adapter.remove(descriptor);
		commitIfAuto();
	}

	@Override
	public Lists lists() {
		ensureOpen();
		assert lists != null;
		return lists;
	}

	@Override
	public Types types() {
		ensureOpen();
		assert types != null;
		return types;
	}

    @Override
    public Properties properties() {
        ensureOpen();
        assert properties != null;
        return properties;
    }

    void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("This connection is closed.");
		}
	}

	void commitIfAuto() throws OntoDriverException {
		if (autoCommit) {
			adapter.commit();
		}
	}
}
