package cz.cvut.kbss.ontodriver.impl.sesame;

import info.aduna.iteration.Iterations;

import java.util.Collection;
import java.util.Set;

import org.openrdf.model.Model;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.LinkedHashModel;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

class CachingStorageProxy extends TransparentStorageProxy {

	private Model explicitModel;
	private Model model;

	public CachingStorageProxy(RepositoryConnection conn) throws OntoDriverException {
		super(conn);

		this.init();
	}

	private void init() throws OntoDriverException {
		try {
			this.explicitModel = Iterations.addAll(conn.getStatements(null, null, null, false),
					new LinkedHashModel());
			this.model = Iterations.addAll(conn.getStatements(null, null, null, true),
					new LinkedHashModel());
		} catch (RepositoryException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			URI context) {
		ensureOpen();
		if (includeInferred) {
			return model.filter(subject, predicate, object, context);
		} else {
			return explicitModel.filter(subject, predicate, object, context);
		}
	}

	@Override
	public Model filter(Resource subject, URI predicate, Value object, boolean includeInferred,
			Collection<URI> contexts) {
		ensureOpen();
		if (includeInferred) {
			return model.filter(subject, predicate, object, SesameUtils.varargs(contexts));
		} else {
			return explicitModel.filter(subject, predicate, object, SesameUtils.varargs(contexts));
		}
	}

	@Override
	public void addStatements(Collection<Statement> statements, URI context) {
		ensureOpen();
		for (Statement stmt : statements) {
			explicitModel.add(stmt.getSubject(), stmt.getPredicate(), stmt.getObject(), context);
			model.add(stmt.getSubject(), stmt.getPredicate(), stmt.getObject(), context);
		}
	}

	@Override
	public void addStatement(Statement statement, URI context) {
		ensureOpen();
		explicitModel.add(statement.getSubject(), statement.getPredicate(), statement.getObject(),
				context);
		model.add(statement.getSubject(), statement.getPredicate(), statement.getObject(), context);
	}

	@Override
	public void removeStatement(Statement statement, URI context) {
		ensureOpen();
		explicitModel.remove(statement.getSubject(), statement.getPredicate(),
				statement.getObject(), (Resource) context);
		model.remove(statement.getSubject(), statement.getPredicate(), statement.getObject(),
				(Resource) context);
	}

	@Override
	public void removeStatements(Collection<Statement> statements, URI context) {
		ensureOpen();
		for (Statement stmt : statements) {
			explicitModel.remove(stmt.getSubject(), stmt.getPredicate(), stmt.getObject(), context);
			model.remove(stmt.getSubject(), stmt.getPredicate(), stmt.getObject(), context);
		}
	}

	@Override
	public boolean contains(URI uri, Set<URI> contexts) {
		ensureOpen();
		final URI[] ctxs = SesameUtils.varargs(contexts);
		return explicitModel.contains(uri, null, null, ctxs)
				|| explicitModel.contains(null, null, uri, ctxs);
	}

	@Override
	public boolean isSubjectOfType(URI subject, URI type) {
		ensureOpen();
		return model.contains(subject, RDF.TYPE, type);
	}
}
