package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.jopa.model.RepositoryID;
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
public class JopaStatement implements Statement {

	private final StorageManager manager;

	private boolean useTransactionalOntology;
	private String query;
	private RepositoryID repository;

	public JopaStatement(StorageManager manager) {
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

	public RepositoryID getRepository() {
		return repository;
	}

	@Override
	public ResultSet executeQuery(String sparql, RepositoryID repository)
			throws OntoDriverException {
		initQuery(sparql, repository);
		return manager.executeStatement(this);
	}

	@Override
	public int executeUpdate(String sparql, RepositoryID repository) throws OntoDriverException {
		initQuery(sparql, repository);
		manager.executeStatement(this);
		// Return 0 for now, we don't known how many statements have been
		// affected
		return 0;
	}

	private void initQuery(String sparql, RepositoryID repository) {
		if (sparql == null || repository == null) {
			throw new NullPointerException();
		}
		this.query = sparql;
		this.repository = repository;
	}

}
