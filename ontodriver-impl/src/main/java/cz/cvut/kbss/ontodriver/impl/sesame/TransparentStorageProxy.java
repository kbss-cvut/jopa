package cz.cvut.kbss.ontodriver.impl.sesame;

import info.aduna.iteration.Iterations;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.LinkedHashModel;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.RepositoryResult;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.exceptions.QueryExecutionException;
import cz.cvut.kbss.ontodriver.exceptions.SesameModuleException;

class TransparentStorageProxy implements StorageProxy {

	protected final RepositoryConnection conn;
	private final Map<URI, URI> persisted = new HashMap<>();
	private boolean open;

	public TransparentStorageProxy(RepositoryConnection conn) throws OntoDriverException {
		assert conn != null : "Argument conn cannot be null.";

		this.conn = conn;
		try {
			conn.begin();
		} catch (RepositoryException e) {
			throw new OntoDriverException(e);
		}
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		this.open = false;
		try {
			if (conn.isActive()) {
				conn.rollback();
			}
			conn.close();
		} catch (RepositoryException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			Set<URI> contexts) {
		ensureOpen();
		RepositoryResult<Statement> rr;
		try {
			rr = conn.getStatements(subject, predicate, object, includeInferred,
					SesameUtils.varargs(contexts));
			return Iterations.addAll(rr, new LinkedHashModel());
		} catch (RepositoryException e) {
			throw new SesameModuleException(e);
		}
	}

	@Override
	public void addStatements(Collection<Statement> statements, URI context) {
		ensureOpen();
		for (Statement stmt : statements) {
			if (stmt.getPredicate().equals(RDF.TYPE)) {
				persisted.put((URI) stmt.getSubject(), context);
			}
		}
		// no-op
	}

	@Override
	public void addStatement(Statement statement, URI context) {
		ensureOpen();
		if (statement.getPredicate().equals(RDF.TYPE)) {
			persisted.put((URI) statement.getSubject(), context);
		}
		// no-op
	}

	@Override
	public void removeStatements(Collection<Statement> statements, URI context) {
		ensureOpen();
		// no-op
	}

	@Override
	public boolean contains(URI uri, Set<URI> contexts) {
		ensureOpen();
		try {
			final URI[] ctxs = SesameUtils.varargs(contexts);
			// The URI is present either as subject or object in the storage or
			// it is among the persisted ones (in the right context)
			return conn.hasStatement(uri, null, null, false, ctxs)
					|| conn.hasStatement(null, null, uri, false, ctxs)
					|| (persisted.containsKey(uri) && contexts.contains(persisted.get(uri)));
		} catch (RepositoryException e) {
			throw new SesameModuleException(e);
		}
	}

	@Override
	public boolean isSubjectOfType(URI subject, URI type) {
		ensureOpen();
		try {
			return conn.hasStatement(subject, RDF.TYPE, type, true);
		} catch (RepositoryException e) {
			throw new SesameModuleException(e);
		}
	}

	@Override
	public TupleQueryResult executeQuery(String query) throws QueryExecutionException {
		ensureOpen();
		TupleQuery tq;
		try {
			tq = conn.prepareTupleQuery(QueryLanguage.SPARQL, query);
			return tq.evaluate();
		} catch (RepositoryException | MalformedQueryException | QueryEvaluationException e) {
			throw new QueryExecutionException(e);
		}
	}

	protected void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The storage proxy is close!");
		}
	}
}
