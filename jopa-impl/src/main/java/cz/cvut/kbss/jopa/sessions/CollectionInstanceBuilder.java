package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

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

	private static final Class<?> singletonListClass = Collections.singletonList(null).getClass();
	private static final Class<?> singletonSetClass = Collections.singleton(null).getClass();
	private static final Class<?> arrayAsListClass = Arrays.asList(new Object()).getClass();

	CollectionInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
		super(builder, uow);
		this.populates = true;
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
	Object buildClone(Object cloneOwner, Field field, Object collection, Descriptor repository)
			throws OWLPersistenceException {
		assert (collection instanceof Collection);
		Collection<?> container = (Collection<?>) collection;
		if (container instanceof IndirectCollection<?>) {
			container = (Collection<?>) ((IndirectCollection<?>) container)
					.getReferencedCollection();
		}
		Collection<?> clone = null;
		clone = cloneUsingDefaultConstructor(cloneOwner, field, container, repository);
		if (clone == null) {
			if (Collections.EMPTY_LIST == container) {
				return Collections.EMPTY_LIST;
			}
			if (Collections.EMPTY_SET == container) {
				return Collections.EMPTY_SET;
			}
			Constructor<?> c = null;
			Object element = container.iterator().next();
			Object[] params = new Object[1];
			if (!CloneBuilderImpl.isPrimitiveOrString(element.getClass())) {
				element = builder.buildClone(element, repository);
				if (element instanceof Collection || element instanceof Map) {
					element = builder.createIndirectCollection(element, cloneOwner, field);
				}
			}
			params[0] = element;
			if (singletonListClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(singletonListClass);
			} else if (singletonSetClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(singletonSetClass);
			} else if (arrayAsListClass.isInstance(container)) {
				c = getFirstDeclaredConstructorFor(arrayAsListClass);
				params[0] = builder.cloneArray(container.toArray(), repository);
			} else {
				throw new OWLPersistenceException("Encountered unsupported type of collection: "
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
		clone = (Collection<?>) builder.createIndirectCollection(clone, cloneOwner, field);
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
	private Collection<?> cloneUsingDefaultConstructor(Object cloneOwner, Field field,
			Collection<?> container, Descriptor repository) {
		Class<?> javaClass = container.getClass();
		Collection<?> result = createNewInstance(javaClass, container.size());
		if (result != null) {
			// Makes shallow copy
			cloneCollectionContent(cloneOwner, field, container, result, repository);
		}
		return result;
	}

	private Collection<?> createNewInstance(Class<?> type, int size) {
		Constructor<?> ctor = null;
		Object[] params = null;
		Class<?>[] types = { int.class };
		// Look for constructor taking initial size as parameter
		ctor = getDeclaredConstructorFor(type, types);
		if (ctor != null) {
			params = new Object[1];
			params[0] = Integer.valueOf(size);
		} else {
			ctor = DefaultInstanceBuilder.getDeclaredConstructorFor(type, null);
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
				// Do nothing
			}
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (InvocationTargetException e) {
			throw new OWLPersistenceException(e);
		}
		return result;
	}

	/**
	 * Clone all the elements in the collection. This will make sure that the
	 * cloning process creates a deep copy.
	 * 
	 * @param source
	 *            The collection to clone.
	 */
	private void cloneCollectionContent(Object cloneOwner, Field field, Collection<?> source,
			Collection<?> target, Descriptor repository) {
		if (source.isEmpty()) {
			return;
		}
		Collection<Object> tg = (Collection<Object>) target;
		for (Object obj : source) {
			if (obj == null) {
				tg.add(obj);
				continue;
			}
			if (CloneBuilderImpl.isPrimitiveOrString(obj.getClass())) {
				tg.addAll(source);
				break;
			}
			Object clone = null;
			if (builder.isTypeManaged(obj.getClass())) {
				clone = uow.registerExistingObject(obj, repository);
			} else {
				clone = builder.buildClone(cloneOwner, field, obj, repository);
			}
			tg.add(clone);
		}
	}

	@Override
	void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue)
			throws IllegalArgumentException, IllegalAccessException {
		assert (originalValue == null || originalValue instanceof Collection);
		assert cloneValue instanceof Collection;

		Collection<Object> orig = (Collection<Object>) originalValue;
		Collection<Object> clone = (Collection<Object>) cloneValue;
		if (clone instanceof IndirectCollection) {
			clone = ((IndirectCollection<Collection<Object>>) clone).getReferencedCollection();
		}
		if (originalValue == null) {
			orig = (Collection<Object>) createNewInstance(clone.getClass(), clone.size());
			if (orig == null) {
				orig = createDefaultCollection(clone.getClass());
			}
			field.set(target, orig);
		}
		orig.clear();
		if (clone.isEmpty()) {
			return;
		}
		final Iterator<Object> it = clone.iterator();
		while (it.hasNext()) {
			final Object cl = it.next();
			orig.add(uow.contains(cl) ? builder.getOriginal(cl) : cl);
		}
		final Types types = field.getAnnotation(Types.class);
		if (types != null) {
			checkForNewTypes(orig);
		}
	}

	private Collection<Object> createDefaultCollection(Class<?> cls) {
		if (Set.class.isAssignableFrom(cls)) {
			return new HashSet<>();
		} else if (List.class.isAssignableFrom(cls)) {
			return new ArrayList<>();
		} else {
			throw new IllegalArgumentException("Unsupported type of collection: " + cls);
		}
	}

	/**
	 * Checks if new types were added to the specified collection. </p>
	 * 
	 * If so, they are added to the module extraction signature managed by
	 * Metamodel.
	 * 
	 * @param collection
	 *            The collection to check
	 * @see Types
	 */
	private void checkForNewTypes(Collection<?> collection) {
		assert collection != null;
		if (collection.isEmpty()) {
			return;
		}
		final Set<URI> signature = builder.getMetamodel().getModuleExtractionExtraSignature();
		for (Object elem : collection) {
			final URI u = EntityPropertiesUtils.getValueAsURI(elem);
			if (!signature.contains(u)) {
				builder.getMetamodel().addUriToModuleExtractionSignature(u);
			}
		}
	}
}
