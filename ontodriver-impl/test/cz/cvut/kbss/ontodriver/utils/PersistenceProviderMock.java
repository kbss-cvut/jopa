package cz.cvut.kbss.ontodriver.utils;

import java.util.Set;

import cz.cvut.kbss.jopa.model.metamodel.EmbeddableType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.ManagedType;
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
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey) {
		return null;
	}

	private static class MetamodelMock implements Metamodel {

		@Override
		public <X> EntityType<X> entity(Class<X> cls) {
			return null;
		}

		@Override
		@Deprecated
		public <X> ManagedType<X> managedType(Class<X> cls) {
			return null;
		}

		@Override
		@Deprecated
		public <X> EmbeddableType<X> embeddable(Class<X> cls) {
			return null;
		}

		@Override
		@Deprecated
		public Set<ManagedType<?>> getManagedTypes() {
			return null;
		}

		@Override
		public Set<EntityType<?>> getEntities() {
			return null;
		}

		@Override
		@Deprecated
		public Set<EmbeddableType<?>> getEmbeddables() {
			return null;
		}

		@Override
		public Set<Class<?>> getInferredClasses() {
			return null;
		}

		@Override
		public boolean shouldUseAspectJ() {
			return false;
		}
	}
}
