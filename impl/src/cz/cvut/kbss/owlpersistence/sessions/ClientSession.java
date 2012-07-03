package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Collections;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;

import cz.cvut.kbss.owlpersistence.accessors.OntologyAccessor;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;

/**
 * ClientSession are bound to a single client and they provide the access to the
 * underlying ontology.
 * 
 * @author kidney
 * 
 */
public class ClientSession extends AbstractSession {
	// ClientSession by mela mit nejakou connection, pres kterou by rovnou
	// mohla pristupovat do ontologie

	protected ServerSession parent;

	/**
	 * Default constructor. Should not be used.
	 */
	public ClientSession() {
		super();
	}

	public ClientSession(ServerSession parent) {
		this.parent = parent;
	}

	@Override
	public UnitOfWork acquireUnitOfWork() {
		UnitOfWork uow = new UnitOfWorkImpl(this);
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork acquired.");
		}
		return uow;
	}

	public ServerSession getParent() {
		return parent;
	}

	public void setParent(ServerSession parent) {
		this.parent = parent;
	}

	@Override
	public void release() {
		// Release connection
		// TODO
	}

	@Override
	public void releaseObjectCache() {
		this.parent.releaseObjectCache();
	}

	public CacheManager getLiveObjectCache() {
		return this.parent.getLiveObjectCache();
	}

	public void removeObjectFromCache(Object object) {
		if (object == null) {
			return;
		}
		getLiveObjectCache().removeObjectFromCache(object);
	}

	@Override
	public OntologyAccessor getOntologyAccessor() {
		return getParent().getOntologyAccessor();
	}

	public Vector<?> executeQuery(String sparqlQuery) {
		return this.parent.executeQuery(sparqlQuery);
	}

	public Vector<?> readAllObjects(Class<?> domainClass) {
		return this.parent.readAllObjects(domainClass);
	}

	public Object readObject(Class<?> domainClass) {
		return this.parent.readObject(domainClass);
	}

	public <T> T readObject(Class<T> cls, Object primaryKey) {
		return this.parent.readObject(cls, primaryKey);
	}

	public Set<Class<?>> getManagedTypes() {
		if (this.parent == null) {
			return Collections.emptySet();
		}
		return this.parent.getManagedTypes();
	}

	@Override
	Metamodel getMetamodel() {
		return parent.getMetamodel();
	}
}
