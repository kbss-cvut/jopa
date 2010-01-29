package cz.cvut.kbss.owlpersistence.model;

public interface PersistenceUtil {

	public boolean isLoaded(Object entity, String attributeName);

	public boolean isLoaded(Object entity);

}
