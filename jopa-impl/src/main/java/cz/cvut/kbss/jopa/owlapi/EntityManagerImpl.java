/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.persistence.EntityTransaction;
import javax.persistence.TransactionRequiredException;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationValueVisitor;
import org.semanticweb.owlapi.model.OWLAnonymousIndividual;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeVisitor;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.model.RemoveImport;
import org.semanticweb.owlapi.model.RemoveOntologyAnnotation;
import org.semanticweb.owlapi.model.SetOntologyID;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import cz.cvut.kbss.jopa.model.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.ic.IntegrityConstraint;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.transactions.EntityTransactionWrapper;
import cz.cvut.kbss.jopa.transactions.TransactionWrapper;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.owl2query.engine.OWL2QueryEngine;
import cz.cvut.kbss.owl2query.model.OWL2Ontology;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3OWL2Ontology;
import cz.cvut.kbss.owl2query.model.owlapi.OWLAPIv3QueryFactory;

public class EntityManagerImpl extends AbstractEntityManager {

	private static final Logger LOG = Logger.getLogger(EntityManagerImpl.class.getName());
	private OWLReasoner r;
	private OWLDataFactory f;
	private OWLOntology workingOnt;
	private OWLOntology reasoningOnt;
	private OWLOntologyManager m;
	private EntityManagerFactoryImpl emf;
	/**
	 * A collection of all entities that are currently managed by the
	 * persistence context. This collection includes also newly added entities
	 * that are not persistent yet.
	 */
	private final Map<Object, OWLNamedIndividual> managed = new HashMap<Object, OWLNamedIndividual>();
	/**
	 * A collection of all entities that are currently removed from the
	 * persistence context.
	 */
	private final Map<Object, OWLNamedIndividual> removed = new HashMap<Object, OWLNamedIndividual>();
	private final List<OWLOntologyChange> allChanges = Collections
			.synchronizedList(new ArrayList<OWLOntologyChange>());
	private boolean open;
	private String lang = "en";

	private TransactionWrapper transaction;
	private UnitOfWorkImpl persistenceContext;
	private ServerSession serverSession;

	public EntityManagerImpl(EntityManagerFactoryImpl emf, Map<String, String> map,
			ServerSession serverSession) {
		this.emf = emf;
		this.serverSession = serverSession;

		this.setTransactionWrapper();

		this.open = true;
	}

	public enum State {
		NEW, MANAGED, DETACHED, REMOVED;
	}

	/**
	 * This method takes an OWLClass instance and saves it to the persistence
	 * context.
	 * 
	 * @throws OWLPersistenceException
	 *             whenever the entity is already persisted.
	 */
	public void persist(final Object entity) {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.config("Persisting " + entity);
		}
		ensureOpen();
		if (entity == null) {
			throw new NullPointerException("Null passed to persist.");
		}

		switch (getState(entity)) {
		case NEW:
			try {
				getCurrentPersistenceContext().registerNewObject(entity);
			} catch (Exception e) {
				if (getTransaction().isActive()) {
					getTransaction().setRollbackOnly();
				}
				throw new OWLPersistenceException("A problem occured when persisting " + entity, e);
			}
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					try {
						Object ox = at.getJavaField().get(o);
						System.out.println("object=" + o + ", attribute=" + at.getName()
								+ ", value=" + ox);

						if (ox == null) {
							return;
						}

						if (at.isCollection()) {
							for (final Object ox2 : (Collection<?>) ox) {
								persist(ox2);
							}
						} else {
							persist(ox);
						}
					} catch (Exception e) {
						if (getTransaction().isActive()) {
							getTransaction().setRollbackOnly();
						}
						throw new OWLPersistenceException(
								"A problem occured when persisting attribute " + at.getName()
										+ " of with value " + o + " of object " + entity, e);
					}
				}
			}.start(this, entity, CascadeType.PERSIST);
			break;
		case DETACHED:
			throw new OWLEntityExistsException("Entity " + entity + " already exists.");
		case REMOVED:
			getCurrentPersistenceContext().revertObject(entity);
			break;
		}
	}

	@Override
	public void persist(final Object entity, final URI contextUri) {
		if (LOG.isLoggable(Level.FINER)) {
			LOG.config("Persisting " + entity);
		}
		ensureOpen();
		if (entity == null || contextUri == null) {
			throw new NullPointerException("Null passed to persist.");
		}

		switch (getState(entity, contextUri)) {
		case NEW:
			try {
				getCurrentPersistenceContext().registerNewObject(entity, contextUri);
			} catch (Exception e) {
				if (getTransaction().isActive()) {
					getTransaction().setRollbackOnly();
				}
				throw new OWLPersistenceException("A problem occured when persisting " + entity, e);
			}
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					try {
						Object ox = at.getJavaField().get(o);
						System.out.println("object=" + o + ", attribute=" + at.getName()
								+ ", value=" + ox);

						if (ox == null) {
							return;
						}

						if (at.isCollection()) {
							for (final Object ox2 : (Collection<?>) ox) {
								persist(ox2, contextUri);
							}
						} else {
							persist(ox, contextUri);
						}
					} catch (Exception e) {
						if (getTransaction().isActive()) {
							getTransaction().setRollbackOnly();
						}
						throw new OWLPersistenceException(
								"A problem occured when persisting attribute " + at.getName()
										+ " of with value " + o + " of object " + entity, e);
					}
				}
			}.start(this, entity, CascadeType.PERSIST);
			break;
		case DETACHED:
			throw new OWLEntityExistsException("Entity " + entity + " already exists.");
		case REMOVED:
			getCurrentPersistenceContext().revertObject(entity);
			break;
		}
	}

	@Override
	public <T> T merge(final T entity) {
		if (entity == null) {
			throw new NullPointerException();
		}
		return mergeInternal(entity, null);
	}

	@Override
	public <T> T merge(final T entity, final URI contextUri) {
		if (entity == null || contextUri == null) {
			throw new NullPointerException();
		}
		return mergeInternal(entity, contextUri);
	}

	/**
	 * Merges state of the specified entity into the current persistence
	 * context. </p>
	 * 
	 * @param entity
	 *            Entity instance
	 * @param contextUri
	 *            URI of ontology context into which the entity should be
	 *            merged. Optional parameter, if not specified, the entity is
	 *            merged into the first ontology context in which it is found
	 * @return Managed instance of the merged entity
	 */
	private <T> T mergeInternal(final T entity, final URI contextUri) {
		assert entity != null;
		if (LOG.isLoggable(Level.FINER)) {
			LOG.config("Merging " + entity);
		}
		ensureOpen();

		Class<T> clz = (Class<T>) entity.getClass();

		switch (getState(entity, contextUri)) {
		case NEW:
			if (contextUri == null) {
				getCurrentPersistenceContext().registerNewObject(entity);
			} else {
				getCurrentPersistenceContext().registerNewObject(entity, contextUri);
			}
			// Intentional case fall-through
		case MANAGED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(entity, mergeInternal(o, contextUri));
				}
			}.start(this, entity, CascadeType.MERGE);
			return entity;
		case DETACHED:
			final T merged;

			if (contextUri == null) {
				merged = getCurrentPersistenceContext().mergeDetached(entity);
			} else {
				merged = getCurrentPersistenceContext().mergeDetached(entity, contextUri);
			}

			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(merged, mergeInternal(o, contextUri));
				}

				@Override
				protected void exploreNonCascaded(Attribute<?, ?> at, Object o)
						throws IllegalAccessException {
					at.getJavaField().set(merged, at.getJavaField().get(o));
				}
			}.start(this, merged, CascadeType.MERGE);
			return merged;
		case REMOVED:
		default:
			throw new IllegalArgumentException();
		}
	}

	public void remove(Object object) {
		ensureOpen();

		switch (getState(object)) {
		case NEW:
			// New objects are also registered, so they have to be removed
			getCurrentPersistenceContext().removeObject(object);
			break;
		case DETACHED:
			throw new IllegalArgumentException();
		case MANAGED:
			getCurrentPersistenceContext().removeObject(object);
		case REMOVED:
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o)
						throws IllegalArgumentException, IllegalAccessException {
					final Object toRemove = at.getJavaField().get(o);
					remove(toRemove);
				}
			}.start(this, object, CascadeType.REMOVE);
			break;
		}

	}

	public <T> T find(Class<T> t, Object primaryKey) {
		if (t == null || primaryKey == null) {
			throw new NullPointerException("Null passed to find. t = " + t + ", primaryKey = "
					+ primaryKey);
		}
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.config("Finding " + t + " with key " + primaryKey);
		}
		final IRI uri = IRI.create(primaryKey.toString());

		T ob = getCurrentPersistenceContext().readObject(t, uri);

		return ob;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, URI contextUri) {
		if (cls == null || primaryKey == null || contextUri == null) {
			throw new NullPointerException("Null passed to find. cls = " + cls + ", primaryKey = "
					+ primaryKey + ", contextUri = " + contextUri);
		}
		ensureOpen();
		if (LOG.isLoggable(Level.FINER)) {
			LOG.config("Finding " + cls + " with key " + primaryKey + " in context " + contextUri);
		}
		final IRI uri = IRI.create(primaryKey.toString());

		T ob = getCurrentPersistenceContext().readObject(cls, uri, contextUri);

		return ob;
	}

	public void flush() {
		ensureOpen();

		if (LOG.isLoggable(Level.FINER)) {
			LOG.config("Flushing ...");
		}
		if (!getTransaction().isActive()) {
			throw new TransactionRequiredException();
		}
		this.getCurrentPersistenceContext().writeUncommittedChanges();
	}

	public void refresh(Object object) {
		ensureOpen();

		switch (getState(object)) {
		case NEW:
		case DETACHED:
		case REMOVED:
			throw new IllegalArgumentException();
		case MANAGED:
			this.getCurrentPersistenceContext().revertObject(object);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					refresh(o);
				}
			}.start(this, object, CascadeType.REFRESH);
		}
	}

	public void clear() {
		managed.clear();
		removed.clear();
		getCurrentPersistenceContext().clear();
	}

	public void detach(Object entity) {
		ensureOpen();

		switch (getState(entity)) {
		case MANAGED:
			getCurrentPersistenceContext().unregisterObject(entity);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					detach(o);
				}
			}.start(this, entity, CascadeType.DETACH);
			break;
		case REMOVED:
			getCurrentPersistenceContext().unregisterObject(entity);
			new OneLevelCascadeExplorer() {
				@Override
				protected void exploreCascaded(Attribute<?, ?> at, Object o) {
					detach(o);
				}
			}.start(this, entity, CascadeType.DETACH);
			break;
		case NEW:
		case DETACHED:
			break;
		}
	}

	public boolean contains(Object entity) {
		ensureOpen();
		return getCurrentPersistenceContext().contains(entity);
	}

	@Override
	public List<Context> getAvailableContexts() {
		return getCurrentPersistenceContext().getContexts();
	}

	/**
	 * Checks whether ontology context is consistent.
	 * 
	 * TODO THis method is not part of the public API, yet
	 * 
	 * @param contextUri
	 *            URI of the context
	 * @return {@code true} if the context is consistent, {@code false}
	 *         otherwise
	 */
	public boolean isContextConsistent(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException();
		}
		return getCurrentPersistenceContext().isContextConsistent(contextUri);
	}

	public void close() {
		ensureOpen();
		removeCurrentPersistenceContext();
		open = false;
	}

	public boolean isOpen() {
		return open;
	}

	public javax.persistence.EntityTransaction getTransaction() {
		return transaction.getTransaction();
	}

	public EntityManagerFactoryImpl getEntityManagerFactory() {
		return emf;
	}

	public Metamodel getMetamodel() {
		return emf.getMetamodel();
	}

	public boolean isLoaded(final Object object, final String attributeName) {
		// TODO
		return false;
	}

	public Query<?> createQuery(String qlString) {
		return getServerSession().createQuery(qlString, this);
	}

	public <T> TypedQuery<T> createQuery(String qlString, Class<T> resultClass) {
		return _createTypedQuery(qlString, resultClass, false);
	}

	public Query<List<String>> createNativeQuery(String sparql) {
		return getServerSession().createNativeQuery(sparql, this);
	}

	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass) {
		return _createTypedQuery(sparql, resultClass, true);
	}

	public <T> T unwrap(Class<T> cls) {
		if (cls.equals(this.getClass())) {
			return cls.cast(this);
		} else if (OWLOntologyManager.class.isAssignableFrom(cls)) {
			return cls.cast(m);
		} else if (OWLDataFactory.class.isAssignableFrom(cls)) {
			return cls.cast(m.getOWLDataFactory());
		} else if (OWLOntology.class.isAssignableFrom(cls)) {
			return cls.cast(workingOnt);
		}

		throw new OWLPersistenceException();
	}

	public Object getDelegate() {
		return unwrap(EntityManagerImpl.class);
	}

	public String getLabel(String iri) {
		String label = null;

		for (final OWLAnnotationAssertionAxiom ax : reasoningOnt.getAnnotationAssertionAxioms(IRI
				.create(iri))) {

			OWLAnnotation a = ax.getAnnotation();
			if (a.getProperty().equals(
					f.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI()))) {
				final LanguageResolverVisitor v = new LanguageResolverVisitor();
				a.getValue().accept(v);

				if (v.getLang().equals(lang)) {
					return v.getValue();
				}

				if (v.getLang().isEmpty() || (v.getLang() == null) || (label == null)) {
					label = v.getValue();
				}
			}
		}

		if (LOG.isLoggable(Level.WARNING)) {
			LOG.warning("No label found for " + iri + ", using IRI itself.");
		}

		return iri;
	}

	class LanguageResolverVisitor implements OWLAnnotationValueVisitor {

		String value = null;
		String lang = null;

		public void visit(OWLAnonymousIndividual arg0) {
			// not supported - silently ignore
		}

		public void visit(IRI arg0) {
			// not supported - silently ignore
		}

		public String getValue() {
			return value;
		}

		public String getLang() {
			return lang;
		}

		public void visit(OWLLiteral sl) {
			value = sl.getLiteral();
			lang = sl.getLang();
		}
	}

	synchronized void addChanges(final Collection<OWLOntologyChange> c) {
		for (final OWLOntologyChange cc : c) {
			if (allChanges.contains(cc)) {
				continue;
			}

			cc.accept(new OWLOntologyChangeVisitor() {

				public void visit(AddAxiom arg0) {
					final RemoveAxiom ax = new RemoveAxiom(workingOnt, arg0.getAxiom());
					if (allChanges.contains(ax)) {
						allChanges.remove(ax);
					} else {
						allChanges.add(arg0);
					}
				}

				public void visit(RemoveAxiom arg0) {
					final AddAxiom ax = new AddAxiom(workingOnt, arg0.getAxiom());
					if (allChanges.contains(ax)) {
						allChanges.remove(ax);
					} else {
						allChanges.add(arg0);
					}
				}

				public void visit(SetOntologyID arg0) {
					throw new UnsupportedOperationException(
							"Changing ontology URI is not supported.");
				}

				public void visit(AddImport arg0) {
					throw new UnsupportedOperationException(
							"Adding ontology Import is not supported.");
				}

				public void visit(RemoveImport arg0) {
					throw new UnsupportedOperationException(
							"Removing ontology Import is not supported.");
				}

				public void visit(AddOntologyAnnotation arg0) {
					throw new UnsupportedOperationException(
							"Adding ontology annotation is not supported.");
				}

				public void visit(RemoveOntologyAnnotation arg0) {
					throw new UnsupportedOperationException(
							"Removing ontology annotation is not supported.");
				}

			});
		}
	}

	synchronized void addChange(final OWLOntologyChange c) {
		addChanges(Collections.singleton(c));
	}

	private void ensureOpen() {
		if (!isOpen()) {
			throw new OWLPersistenceException("The entity manager is closed !");
		}
	}

	private State getState(Object entity) {
		return getCurrentPersistenceContext().getState(entity);
	}

	private State getState(Object entity, URI contextUri) {
		return getCurrentPersistenceContext().getState(entity, contextUri);
	}

	@Override
	protected void finalize() throws Throwable {
		if (isOpen()) {
			close();
		}
	}

	private <T> TypedQuery<T> _createTypedQuery(String string, Class<T> cls, boolean sparql) {
		return getServerSession().createQuery(string, cls, sparql, this);
	}

	class ICEvaluator {
		public boolean isSatisfied(IntegrityConstraint check) {

			OWL2Ontology<OWLObject> ont = new OWLAPIv3OWL2Ontology(m, workingOnt, r);
			OWLAPIv3QueryFactory fact = new OWLAPIv3QueryFactory(m, workingOnt);

			ICQueryGenerator v = new ICQueryGenerator(fact, ont);
			check.accept(v);

			return OWL2QueryEngine.exec(v.getQuery()).isEmpty();
		}

	}

	public UnitOfWorkImpl getCurrentPersistenceContext() {
		if (this.persistenceContext == null) {
			this.persistenceContext = (UnitOfWorkImpl) this.serverSession.acquireClientSession()
					.acquireUnitOfWork();
			persistenceContext.setEntityManager(this);
		}
		return this.persistenceContext;
	}

	public ServerSession getServerSession() {
		return this.emf.getServerSession();
	}

	/**
	 * Called from EntityTransaction in case of a rollback. Releasing the UoW is
	 * up to the EntityTransaction.
	 */
	public void removeCurrentPersistenceContext() {
		if (persistenceContext != null && persistenceContext.isActive()) {
			persistenceContext.release();
		}
		this.persistenceContext = null;
	}

	public void transactionStarted(EntityTransaction t) {
		this.serverSession.transactionStarted(t, this);
	}

	public void transactionFinished(EntityTransaction t) {
		this.serverSession.transactionFinished(t);
	}

	/**
	 * Since we support only EntityTransactions, we set the TransactionWrapper
	 * to EntityTransactionWrapper. In the future, if JTA transactions are
	 * supported, JTATransactionWrapper should be set instead of the
	 * EntityTransactionWrapper.
	 */
	private void setTransactionWrapper() {
		this.transaction = new EntityTransactionWrapper(this);
	}
}
