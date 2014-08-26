package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.ontodriver.Statement;

public interface ConnectionWrapper {

	boolean contains(Object primaryKey, Descriptor descriptor);

	<T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor);

	<T> void merge(T entity, Field field, Descriptor repository);

	<T> void persist(Object primaryKey, T entity, Descriptor descriptor);

	<T> void remove(Object primaryKey, Descriptor descriptor);

	<T> void loadFieldValue(T entity, Field field, Descriptor descriptor);

	void commit();

	void rollback();

	void close();

	boolean isConsistent(URI context);

	List<URI> getContexts();
	
	Statement createStatement();
}
