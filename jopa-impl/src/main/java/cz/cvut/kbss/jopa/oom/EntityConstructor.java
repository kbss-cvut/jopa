package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.util.Collection;

import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class EntityConstructor {

	private final UnitOfWorkImpl uow;

	public EntityConstructor(UnitOfWorkImpl uow) {
		this.uow = uow;
	}

	<T> T reconstructEntity(Class<T> cls, Object primaryKey, Collection<Axiom> axioms,
			EntityType<T> et) {
		// TODO
		return null;
	}

	<T> void setFieldValue(T entity, Field field, Collection<Axiom> axioms, EntityType<T> et) {
		// TODO
	}
}
