package cz.cvut.kbss.ontodriver.utils;

import java.net.URI;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;

public class PersistenceProviderMock implements PersistenceProviderFacade {

	private final Metamodel mock;

	public PersistenceProviderMock() {
		this.mock = new MetamodelMock();
	}

	@Override
	public Metamodel getMetamodel() {
		return mock;
	}

	@Override
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey, URI contextUri) {
		return null;
	}
}
