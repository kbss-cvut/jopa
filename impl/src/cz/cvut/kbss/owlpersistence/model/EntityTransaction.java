package cz.cvut.kbss.owlpersistence.model;

public interface EntityTransaction {

	public void begin();

	public void commit();

	public void rollback();

	public void setRollbackOnly();

	public boolean getRollbackOnly();

	public boolean isActive();
}
