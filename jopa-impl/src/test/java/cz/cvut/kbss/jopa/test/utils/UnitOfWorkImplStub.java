package cz.cvut.kbss.jopa.test.utils;

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.sessions.AbstractSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class UnitOfWorkImplStub extends UnitOfWorkImpl {

	private Object lastEntity;

	public UnitOfWorkImplStub(AbstractSession parent) {
		super(parent);

	}

	/**
	 * This is the only method we need to override.
	 */
	@Override
	public void attributeChanged(Object entity, Field field) {
		this.lastEntity = entity;
	}

	public Object getLastEntity() {
		return lastEntity;
	}

	public void setLastEntity(Object entity) {
		this.lastEntity = entity;
	}

	@Override
	public boolean isInTransaction() {
		return true;
	}
}
