package cz.cvut.kbss.jopa.owlapi.utils;

import java.lang.reflect.Field;
import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.accessors.TransactionOntologyAccessor;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

public class AccessorStub implements TransactionOntologyAccessor {
	public void persistEntity(Object entity, UnitOfWork uow) {
	}

	public void removeEntity(Object entity) {
	}

	public <T> T readEntity(Class<T> cls, Object uri) {
		return null;
	}

	public void writeChanges(List<OWLOntologyChange> changes) {
	}

	public void writeChange(OWLOntologyChange change) {
	}

	public void mergeToWorkingOntology() {
	}

	public boolean isInOntologySignature(IRI uri, boolean searchImports) {
		return false;
	}

	public OWLNamedIndividual getOWLNamedIndividual(IRI identifier) {
		return null;
	}

	/**
	 * This is the only method we need.
	 */
	public IRI getIdentifier(Object object) {
		OWLClassB ob = (OWLClassB) object;
		return IRI.create(ob.getUri());
	}

	public void persistExistingEntity(Object entity, UnitOfWork uow) {
	}

	public Query<?> createQuery(String qlString, final EntityManager em) {
		return null;
	}

	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass, boolean sparql,
			final EntityManager em) {
		return null;
	}

	public Query<List<String>> createNativeQuery(String sqlString, final EntityManager em) {
		return null;
	}

	public void close() {
	}

	public void generateNewIRI(Object entity) {
	}

	public boolean isOpen() {
		return true;
	}

	public void loadReference(Object entity, Field field, UnitOfWork uow) {
	}
}
