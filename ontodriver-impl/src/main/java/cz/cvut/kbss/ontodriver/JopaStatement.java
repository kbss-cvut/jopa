package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
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

	private final StorageModule storageModule;

	private boolean useTransactionalOntology;
	private String query;
	private URI[] contexts;

	public JopaStatement(StorageModule storageModule) {
		this.storageModule = Objects.requireNonNull(storageModule,
				ErrorUtils.constructNPXMessage("storageModule"));
		this.useTransactionalOntology = true;
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

	public URI[] getContexts() {
		return contexts;
	}

	@Override
	public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
		initQuery(sparql, contexts);
		return storageModule.executeStatement(this);
	}

	@Override
	public int executeUpdate(String sparql, URI... contexts) throws OntoDriverException {
		initQuery(sparql, contexts);
		storageModule.executeStatement(this);
		// Return 0 for now, we don't known how many statements have been
		// affected
		return 0;
	}

	private void initQuery(String sparql, URI... contexts) {
		Objects.requireNonNull(sparql, ErrorUtils.constructNPXMessage("sparql"));
		this.query = sparql;
		this.contexts = contexts;
	}
}
