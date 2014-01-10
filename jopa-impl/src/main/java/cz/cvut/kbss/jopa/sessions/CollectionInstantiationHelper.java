package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

/**
 * Special class for cloning collections. Introduced because some Java
 * collection have no no-argument constructor and thus they must be cloned
 * specially. NOTE: This class may be removed in case a better cloning
 * mechanisms (namely database mappings and copy policies) is introduced.
 * 
 * @author kidney
 * 
 */
public class CollectionInstantiationHelper {

	private InstantiationHelper builder;
	private static final Class<?> singletonListClass = Collections.singletonList(null).getClass();
	private static final Class<?> singletonSetClass = Collections.singleton(null).getClass();
	private static final Class<?> singletonMapClass = Collections.singletonMap(null, null)
			.getClass();
	private static final Class<?> arrayAsListClass = Arrays.asList(new Object()).getClass();

	public CollectionInstantiationHelper() {
		this.builder = null;
	}

	public CollectionInstantiationHelper(InstantiationHelper builder) {
		super();
		this.builder = builder;
	}

	/**
	 * This method is the entry point for cloning the Java collections. It
	 * clones standard collections as well as immutable collections and
	 * singleton collections. TODO Add support for maps
	 * 
	 * @param collection
	 *            The collection to clone.
	 * @return A deep clone of the specified collection.
	 */
	public Object buildNewInstance(Object collection, URI contextUri)
			throws OWLPersistenceException {
		Collection<?> container = (Collection<?>) collection;
		Collection<?> clone = null;
		clone = cloneUsingDefaultConstructor(container, contextUri);
		if (clone == null) {
			Constructor<?> c = null;
			final Object element = container.iterator().next();
			Object[] params = new Object[1];
			params[0] = element;
			if (singletonListClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(singletonListClass);
			} else if (singletonSetClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(singletonSetClass);
			} else if (singletonMapClass.isInstance(container)) {
				throw new UnsupportedOperationException("Maps are not supported yet.");
			} else if (arrayAsListClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(arrayAsListClass);
				params[0] = builder.getCloneBuilder().cloneArray(container.toArray(), contextUri);
			} else {
				throw new OWLPersistenceException("Encountered unsupported type of collection: " + container.getClass());
			}
			try {
				if (!c.isAccessible()) {
					c.setAccessible(true);
				}
				clone = (Collection<?>) c.newInstance(params);
			} catch (InstantiationException e) {
				e.printStackTrace();
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				try {
					clone = (Collection<?>) AccessController
							.doPrivileged(new PrivilegedInstanceCreator(c));
				} catch (PrivilegedActionException ex) {
					ex.printStackTrace();
					throw new OWLPersistenceException(ex);
				}
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
				throw new OWLPersistenceException(e);
			} catch (InvocationTargetException e) {
				e.printStackTrace();
				throw new OWLPersistenceException(e);
			}
		}
		return clone;
	}

	/**
	 * Clones the specified collection using its default zero argument
	 * constructor. If the specified collection has none (e. g. like
	 * SingletonList), this method returns null.
	 * 
	 * @param container
	 *            The collection to clone.
	 * @return
	 */
	private Collection<?> cloneUsingDefaultConstructor(Collection<?> container, URI contextUri) {
		Class<?> javaClass = container.getClass();
		Constructor<?> ctor = InstantiationHelper.getDeclaredConstructorFor(javaClass, null);
		if (ctor != null) {
			Collection<?> result = null;
			try {
				result = (Collection<?>) ctor.newInstance((Object[]) null);
			} catch (InstantiationException e) {
				e.printStackTrace();
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				try {
					result = (Collection<?>) AccessController
							.doPrivileged(new PrivilegedInstanceCreator(ctor));
				} catch (PrivilegedActionException ex) {
					return null;
				}
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
				throw new OWLPersistenceException(e);
			} catch (InvocationTargetException e) {
				e.printStackTrace();
				throw new OWLPersistenceException(e);
			}
			result.addAll(cloneCollectionContent(container, contextUri)); // Makes
																			// shallow
			// copy
			return result;
		} else {
			return null;
		}
	}

	/**
	 * Clone all the elements in the collection. This will make sure that the
	 * cloning process creates a deep copy. TODO Add support for maps
	 * 
	 * @param collection
	 *            The collection to clone.
	 */
	private Collection cloneCollectionContent(Collection<?> collection, URI contextUri) {
		Collection<Object> result = null;
		if (collection instanceof Map) {
			throw new UnsupportedOperationException("Maps are not supported yet.");
		} else {
			result = new ArrayList<Object>(collection.size());
			for (Object obj : collection) {
				if (CloneBuilderImpl.isPrimitiveOrString(obj.getClass())) {
					result.addAll(collection);
					break;
				}
				Object clone = null;
				if (builder.getUnitOfWork().getManagedTypes().contains(obj.getClass())) {
					clone = builder.getUnitOfWork().registerExistingObject(obj, contextUri);
				} else {
					clone = builder.getCloneBuilder().buildClone(obj, contextUri);
				}
				result.add(clone);
			}
		}
		return result;
	}

	/**
	 * This helper method returns the first declared constructor of the
	 * specified class. It may be used only in cases when the caller knows
	 * exactly which constructor is the first one declared by the class. A use
	 * case may be a class with only one declared constructor, which is not a
	 * zero argument one.
	 * 
	 * @param javaClass
	 *            The class whose constructors should be searched.
	 * @return The first declared constructor of the specified class.
	 */
	private static Constructor<?> getFirstDeclaredConstructorFor(Class<?> javaClass) {
		Constructor<?>[] ctors = javaClass.getDeclaredConstructors();
		return ctors[0];
	}

}
