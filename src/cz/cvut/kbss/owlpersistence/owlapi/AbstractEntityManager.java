package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;

import cz.cvut.kbss.owlpersistence.model.EntityManager;

public abstract class AbstractEntityManager implements EntityManager {
	public abstract void saveReference(Object object, Field field);

	public abstract void loadReference(Object object, Field field);

	public abstract boolean isLoaded(final Object object,
			final String attributeName);

}
