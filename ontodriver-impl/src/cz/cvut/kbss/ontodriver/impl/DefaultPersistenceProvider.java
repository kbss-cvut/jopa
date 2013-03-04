package cz.cvut.kbss.ontodriver.impl;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.PersistenceProvider;

/**
 * This class is a default implementation of the {@code PersistenceProvider}
 * interface. </p>
 * 
 * It does not support cache access and may return null for metamodel requests.
 * 
 * @author kidney
 * 
 */
class DefaultPersistenceProvider implements PersistenceProvider {

	private final Metamodel metamodel;

	public DefaultPersistenceProvider(Metamodel metamodel) {
		super();
		this.metamodel = metamodel;
	}

	@Override
	public Metamodel getMetamodel() {
		return metamodel;
	}

	/**
	 * Returns always null.
	 * 
	 * @return {@code null}
	 */
	@Override
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey) {
		return null;
	}

}
