package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.sessions.CloneBuilder;

/**
 * This class has responsibility for creating new instances of various kinds of
 * objects. It handles security restrictions as well.
 * 
 * @author kidney
 * 
 */
public class InstantiationHelper {

	private final CloneBuilder builder;
	private final CollectionInstantiationHelper collectionBuilder;
	private final UnitOfWork uow;

	public InstantiationHelper() {
		this.builder = null;
		this.collectionBuilder = null;
		this.uow = null;
	}

	public InstantiationHelper(CloneBuilder builder, UnitOfWork uow) {
		super();
		this.builder = builder;
		this.uow = uow;
		this.collectionBuilder = new CollectionInstantiationHelper(this);
	}

	/**
	 * Builds a new instance of the specified class.
	 * 
	 * @param javaClass
	 * @return New object of the given class.
	 */
	public Object buildNewInstance(final Class<?> javaClass, Object original) {
		if (javaClass == null || original == null) {
			return null;
		}
		if (original instanceof Collection) {
			// According to eclipselink, instanceof is much faster than
			// isAssignableFrom
			return collectionBuilder.buildNewInstance(original);
		}
		Object newInstance = buildNewInstanceUsingDefaultConstructor(javaClass);
		if (newInstance == null) {
			final Field[] fields = javaClass.getDeclaredFields();
			List<Class<?>> fieldClasses = new ArrayList<Class<?>>();
			Constructor<?> c = null;
			try {
				for (Field f : fields) {
					if (isFieldStatic(f)) {
						continue;
					}
					Class<?>[] args = { f.getType() };
					c = getDeclaredConstructorFor(javaClass, args);
					if (c == null) {
						fieldClasses.add(f.getType());
					} else {
						try {
							Object[] params = new Object[1];
							params[0] = original.getClass().getDeclaredField(
									f.getName());
							newInstance = c.newInstance(params);
							if (newInstance != null) {
								return newInstance;
							}
						} catch (SecurityException e) {
							try {
								newInstance = AccessController
										.doPrivileged(new PrivilegedInstanceCreator(
												c));
							} catch (PrivilegedActionException ex) {
								throw new OWLPersistenceException(ex);
							}
							if (newInstance != null) {
								return newInstance;
							}
						} catch (NoSuchFieldException e) {
							throw new OWLPersistenceException(e);
						}
					}
				}
				if (newInstance == null) {
					Class<?>[] args = new Class<?>[fieldClasses.size()];
					args = fieldClasses.toArray(args);
					c = getDeclaredConstructorFor(javaClass, args);
					if (c != null) {
						Object[] params = new Object[args.length];
						for (int i = 0; i < params.length; i++) {
							params[i] = null;
						}
						try {
							newInstance = c.newInstance(params);
						} catch (SecurityException e) {
							try {
								newInstance = AccessController
										.doPrivileged(new PrivilegedInstanceCreator(
												c));
							} catch (PrivilegedActionException ex) {
								throw new OWLPersistenceException(ex);
							}
						}
					}
				}
			} catch (InstantiationException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalArgumentException e) {
				throw new OWLPersistenceException(e);
			} catch (InvocationTargetException e) {
				throw new OWLPersistenceException(e);
			}
		}
		if (newInstance == null) {
			throw new OWLPersistenceException(
					"Unable to create a new object or to find a suitable constructor for class "
							+ javaClass.getName());
		}
		return newInstance;
	}

	/**
	 * Builds a new instance of the specified class, using its no-argument
	 * constructor.
	 * 
	 * @param javaClass
	 * @return New object of the given class, or null if the class has no
	 *         no-argument constructor.
	 */
	private Object buildNewInstanceUsingDefaultConstructor(
			final Class<?> javaClass) {
		final Constructor<?> c = getDeclaredConstructorFor(javaClass, null);
		Object newInstance = null;
		if (c != null) {
			try {
				newInstance = c.newInstance((Object[]) null);
			} catch (SecurityException e) {
				try {
					newInstance = AccessController
							.doPrivileged(new PrivilegedInstanceCreator(c));
				} catch (PrivilegedActionException ex) {
					return null;
				}
			} catch (InstantiationException e) {
				return null;
			} catch (IllegalAccessException e) {
				return null;
			} catch (IllegalArgumentException e) {
				return null;
			} catch (InvocationTargetException e) {
				return null;
			}
		}
		return newInstance;
	}

	UnitOfWork getUnitOfWork() {
		return uow;
	}
	
	CloneBuilderImpl getCloneBuilder() {
		return (CloneBuilderImpl) builder;
	}

	/**
	 * Return the declared constructor for the specified class. If the
	 * constructor is not accessible, it is set accessible. If there is no
	 * constructor corresponding to the specified argument list, null is
	 * returned.
	 * 
	 * @param javaClass
	 *            The class of the constructor.
	 * @param args
	 *            An Array of classes, which should take the constructor as
	 *            parameters.
	 * @return Constructor<?>
	 * @throws SecurityException
	 *             If the security check denies access to the constructor.
	 */
	public static Constructor<?> getDeclaredConstructorFor(
			final Class<?> javaClass, Class<?>[] args) throws SecurityException {
		Constructor<?> c = null;
		try {
			c = javaClass.getDeclaredConstructor(args);
			if (c == null) {
				return null;
			}
			if (!c.isAccessible()) {
				c.setAccessible(true);
			}
		} catch (NoSuchMethodException e) {
			return null;
		}
		return c;
	}

	/**
	 * Checks if the specified field was declared static in its class.
	 * 
	 * @param f
	 *            The field to examine.
	 * @return True when the Field is static.
	 */
	private static boolean isFieldStatic(final Field f) {
		return Modifier.isStatic(f.getModifiers());
	}
}
