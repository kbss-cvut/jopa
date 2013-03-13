package cz.cvut.kbss.ontodriver.utils;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;

public class PersistenceProviderMock implements PersistenceProviderFacade {

	public PersistenceProviderMock() {
	}

	@Override
	public Metamodel getMetamodel() {
		return null;
	}

	@Override
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey) {
		return null;
	}

}
