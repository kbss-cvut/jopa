package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
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
class CollectionInstanceBuilder extends AbstractInstanceBuilder {

	private static final Class<?> singletonListClass = Collections
			.singletonList(null).getClass();
	private static final Class<?> singletonSetClass = Collections.singleton(
			null).getClass();
	private static final Class<?> arrayAsListClass = Arrays
			.asList(new Object()).getClass();

	CollectionInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
		super(builder, uow);
	}

	/**
	 * This method is the entry point for cloning the Java collections. It
	 * clones standard collections as well as immutable collections and
	 * singleton collections. </p>
	 * 
	 * Currently supported are List and Set.
	 * 
	 * @param collection
	 *            The collection to clone
	 * @return A deep clone of the specified collection
	 */
	@Override
	Object buildClone(Class<?> origCls, Object collection, URI contextUri)
			throws OWLPersistenceException {
		assert (collection instanceof Collection);
		Collection<?> container = (Collection<?>) collection;
		if (container instanceof IndirectCollection<?>) {
			container = (Collection<?>) ((IndirectCollection<?>) container)
					.getReferencedCollection();
		}
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
			} else if (arrayAsListClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(arrayAsListClass);
				params[0] = builder.cloneArray(container.toArray(), contextUri);
			} else {
				throw new OWLPersistenceException(
						"Encountered unsupported type of collection: "
								+ container.getClass());
			}
			try {
				if (!c.isAccessible()) {
					c.setAccessible(true);
				}
				clone = (Collection<?>) c.newInstance(params);
			} catch (InstantiationException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				try {
					clone = (Collection<?>) AccessController
							.doPrivileged(new PrivilegedInstanceCreator(c));
				} catch (PrivilegedActionException ex) {
					throw new OWLPersistenceException(ex);
				}
			} catch (IllegalArgumentException e) {
				throw new OWLPersistenceException(e);
			} catch (InvocationTargetException e) {
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
	private Collection<?> cloneUsingDefaultConstructor(Collection<?> container,
			URI contextUri) {
		Class<?> javaClass = container.getClass();
		Constructor<?> ctor = null;
		Object[] params = null;
		Class<?>[] types = { Integer.class };
		// Look for constructor taking initial size as parameter
		ctor = getDeclaredConstructorFor(javaClass, types);
		if (ctor != null) {
			params = new Object[1];
			params[0] = Integer.valueOf(container.size());
		} else {
			ctor = DefaultInstanceBuilder.getDeclaredConstructorFor(javaClass,
					null);
		}
		if (ctor == null) {
			return null;
		}
		Collection<?> result = null;
		try {
			result = (Collection<?>) ctor.newInstance(params);
		} catch (InstantiationException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			try {
				result = (Collection<?>) AccessController
						.doPrivileged(new PrivilegedInstanceCreator(ctor));
			} catch (PrivilegedActionException ex) {
				return null;
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (InvocationTargetException e) {
			throw new OWLPersistenceException(e);
		}
		// Makes shallow copy
		cloneCollectionContent(container, result, contextUri);
		return result;
	}

	/**
	 * Clone all the elements in the collection. This will make sure that the
	 * cloning process creates a deep copy.
	 * 
	 * @param source
	 *            The collection to clone.
	 */
	private void cloneCollectionContent(Collection<?> source,
			Collection<?> target, URI contextUri) {
		if (source.isEmpty()) {
			return;
		}
		Collection<Object> tg = (Collection<Object>) target;
		for (Object obj : source) {
			if (CloneBuilderImpl.isPrimitiveOrString(obj.getClass())) {
				tg.addAll(source);
				break;
			}
			Object clone = null;
			if (builder.isTypeManaged(obj.getClass())) {
				clone = uow.registerExistingObject(obj, contextUri);
			} else {
				clone = builder.buildClone(obj, contextUri);
			}
			tg.add(clone);
		}
	}
}
