package cz.cvut.kbss.ontodriver.sesame;

import java.net.URI;
import java.util.Objects;

import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.TupleQueryResult;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;

public class SesameStatement implements Statement {

	protected boolean useTransactionalOntology;
	protected final StatementExecutor queryExecutor;
	protected ResultSet resultSet;

	private boolean open;

	public SesameStatement(StatementExecutor queryExecutor) {
		this.queryExecutor = queryExecutor;
		this.open = true;
	}

	@Override
	public ResultSet executeQuery(String sparql, URI... contexts) throws OntoDriverException {
		ensureOpen();
		validateQueryParams(sparql);
		final TupleQueryResult tqr = queryExecutor.executeQuery(sparql);
		try {
			this.resultSet = new SesameResultSet(tqr, this);
		} catch (QueryEvaluationException e) {
			throw new SesameDriverException(e);
		}
		return resultSet;
	}

	@Override
	public void executeUpdate(String sparql, URI... contexts) throws OntoDriverException {
		ensureOpen();
		validateQueryParams(sparql);
		queryExecutor.executeUpdate(sparql);
	}

	private void validateQueryParams(String sparql) {
		Objects.requireNonNull(sparql, ErrorUtils.constructNPXMessage("sparql"));
		if (sparql.isEmpty()) {
			throw new IllegalArgumentException("Query string cannot be empty.");
		}
	}

	public void close() throws Exception {
		if (!open) {
			return;
		}
		this.open = false;
		if (resultSet != null) {
			resultSet.close();
			this.resultSet = null;
		}
	}

	public void setUseTransactionalOntology() {
		ensureOpen();
		this.useTransactionalOntology = true;
	}

	public boolean useTransactionalOntology() {
		ensureOpen();
		return useTransactionalOntology;
	}

	public void setUseBackupOntology() {
		ensureOpen();
		this.useTransactionalOntology = false;
	}

	public boolean useBackupOntology() {
		ensureOpen();
		return (!useTransactionalOntology);
	}

	protected void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("This statement is closed.");
		}
	}
}
