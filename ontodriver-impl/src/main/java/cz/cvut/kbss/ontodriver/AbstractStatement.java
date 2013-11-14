package cz.cvut.kbss.ontodriver;

import java.net.URI;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * This class defines the common methods for all Statement implementations. </p>
 * 
 * The implementations will need to deal with different ontology frameworks, e.
 * g. OWLAPI, Jena, Sesame.
 * 
 * @author ledvima1
 * 
 */
public class AbstractStatement implements Statement {

	private final StorageManager manager;

	protected boolean useTransactionalOntology;
	protected String query;
	protected URI context;

	public AbstractStatement(StorageManager manager) {
		if (manager == null) {
			throw new NullPointerException();
		}
		this.useTransactionalOntology = true;
		this.manager = manager;
	}

	@Override
	public void setUseTransactionalOntology() {
		this.useTransactionalOntology = true;
	}

	@Override
	public boolean useTransactionalOntology() {
		return useTransactionalOntology;
	}

	@Override
	public void setUseBackupOntology() {
		this.useTransactionalOntology = false;
	}

	@Override
	public boolean useBackupOntology() {
		return !useTransactionalOntology;
	}

	public String getQuery() {
		return query;
	}

	public URI getContext() {
		return context;
	}

	@Override
	public ResultSet executeQuery(String sparql, URI contextUri) throws OntoDriverException {
		initQuery(sparql, contextUri);
		return manager.executeStatement(this);
	}

	@Override
	public int executeUpdate(String sparql, URI contextUri) throws OntoDriverException {
		initQuery(sparql, contextUri);
		manager.executeStatement(this);
		// Return 0 for now, we don't known how many statements have been
		// affected
		return 0;
	}

	private void initQuery(String sparql, URI context) {
		if (sparql == null || context == null) {
			throw new NullPointerException();
		}
		this.query = sparql;
		this.context = context;
	}

}
