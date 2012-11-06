package cz.cvut.kbss.jopa.owlapi.utils;

import cz.cvut.kbss.jopa.sessions.AbstractSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class UnitOfWorkImplStub extends UnitOfWorkImpl {

	private Object lastEntity;

	public UnitOfWorkImplStub(AbstractSession parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	/**
	 * This is the only method we need to override.
	 */
	@Override
	public void persistChangeInTransaction(Object entity) {
		this.lastEntity = entity;
	}

	public Object getLastEntity() {
		return lastEntity;
	}

}
