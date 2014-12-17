package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

/**
 * This class has responsibility for creating new instances of various kinds of
 * objects. It handles security restrictions as well.
 * 
 * @author kidney
 * 
 */
class DefaultInstanceBuilder extends AbstractInstanceBuilder {

	DefaultInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
		super(builder, uow);
	}

	/**
	 * Builds a new instance of the specified class.
	 * 
	 * @param javaClass
	 * @return New object of the given class.
	 */
	@Override
	Object buildClone(Object cloneOwner, Field field, Object original, Descriptor repository) {
		if (original == null) {
			return null;
		}
		final Class<?> javaClass = original.getClass();
		if (CloneBuilderImpl.isPrimitiveOrString(javaClass)) {
			return original;
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
							params[0] = original.getClass().getDeclaredField(f.getName());
							newInstance = c.newInstance(params);
							return newInstance;
						} catch (SecurityException e) {
							try {
								newInstance = AccessController
										.doPrivileged(new PrivilegedInstanceCreator(c));
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
                                    .doPrivileged(new PrivilegedInstanceCreator(c));
                        } catch (PrivilegedActionException ex) {
                            throw new OWLPersistenceException(ex);
                        }
                    }
                }
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
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

	@Override
	void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue)
			throws IllegalArgumentException, IllegalAccessException {
		if (originalValue == null) {
			Object clOrig = builder.getOriginal(cloneValue);
			if (clOrig == null) {
				clOrig = cloneValue;
			}
			field.set(target, clOrig);
			return;
		}
		Class<?> cls = originalValue.getClass();
		List<Field> fields = EntityPropertiesUtils.getAllFields(cls);
		for (Field f : fields) {
			if (!f.isAccessible()) {
				f.setAccessible(true);
			}
			Object clVal = f.get(cloneValue);
			Object origVal = f.get(originalValue);
			if (!(clVal instanceof Collection) && !builder.isOriginalInUoW(origVal)) {
				f.set(originalValue, clVal);
			} else {
				builder.getInstanceBuilder(origVal).mergeChanges(f, originalValue, origVal, clVal);
			}
		}
	}

	/**
	 * Builds a new instance of the specified class, using its no-argument
	 * constructor.
	 * 
	 * @param javaClass
	 * @return New object of the given class, or null if the class has no
	 *         no-argument constructor.
	 */
	private Object buildNewInstanceUsingDefaultConstructor(final Class<?> javaClass) {
		final Constructor<?> c = getDeclaredConstructorFor(javaClass, null);
		Object newInstance = null;
		if (c != null) {
			try {
				try {
					newInstance = c.newInstance((Object[]) null);
				} catch (SecurityException e) {
					try {
						newInstance = AccessController
								.doPrivileged(new PrivilegedInstanceCreator(c));
					} catch (PrivilegedActionException ex) {
						return null;
					}
				}
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException e) {
				// Do nothing
			}
		}
		return newInstance;
	}
}
