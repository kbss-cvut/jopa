package cz.cvut.kbss.jopa.ontodriver;

import java.net.URI;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;

public class PersistenceProviderFacadeMock implements PersistenceProviderFacade {

	private final Metamodel metamodel;

	public PersistenceProviderFacadeMock(Metamodel metamodel) {
		this.metamodel = metamodel;
	}

	@Override
	public Metamodel getMetamodel() {
		return metamodel;
	}

	@Override
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey, URI contextUri) {
		return null;
	}

}
