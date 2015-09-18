package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.security.PrivilegedActionException;
import java.util.Arrays;
import java.util.logging.Logger;

abstract class AbstractInstanceBuilder {

    protected static final Logger LOG = Logger.getLogger(AbstractInstanceBuilder.class.getName());

    protected boolean populates;
    protected final CloneBuilderImpl builder;
    protected final UnitOfWork uow;

    AbstractInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
        super();
        this.builder = builder;
        this.uow = uow;
        this.populates = false;
    }

    /**
     * Returns true if this builder instances automatically populates the created instance's attribute.
     *
     * @return boolean
     */
    boolean populatesAttributes() {
        return populates;
    }

    /**
     * Builds new instance from the original. </p>
     * <p>
     * For some implementations this may mean creating an empty object, others might choose to initialize it using the
     * original data.
     *
     * @param cloneOwner Instance owning the clone which will be created
     * @param field      Field which will contain the clone
     * @param original   The original object
     * @param descriptor Entity origin
     * @return The clone
     */
    abstract Object buildClone(Object cloneOwner, Field field, Object original,
                               Descriptor descriptor);

    /**
     * Merges changes from clone to the original.
     *
     * @param field         The field we are merging
     * @param target        target object on which the values are merged
     * @param originalValue The original value
     * @param cloneValue    The clone value
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     */
    abstract void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue)
            throws IllegalArgumentException, IllegalAccessException;

    /**
     * Return the declared constructor for the specified class. If the constructor is not accessible, it is set
     * accessible. If there is no constructor corresponding to the specified argument list, null is returned.
     *
     * @param javaClass The class of the constructor.
     * @param args      An Array of classes, which should take the constructor as parameters.
     * @return Constructor
     * @throws SecurityException If the security check denies access to the constructor.
     */
    protected static Constructor<?> getDeclaredConstructorFor(final Class<?> javaClass,
                                                              Class<?>[] args) throws SecurityException {
        Constructor<?> c;
        try {
            c = javaClass.getDeclaredConstructor(args);
            if (c == null) {
                return null;
            }
            if (!c.isAccessible()) {
                c.setAccessible(true);
            }
        } catch (NoSuchMethodException e) {
            LOG.warning("Constructor in type " + javaClass + "  taking arguments " + Arrays.toString(args) +
                    " not found. " + e);
            return null;
        }
        return c;
    }

    /**
     * This helper method returns the first declared constructor of the specified class. It may be used only in cases
     * when the caller knows exactly which constructor is the first one declared by the class. A use case may be a class
     * with only one declared constructor, which is not a zero argument one.
     *
     * @param javaClass The class whose constructors should be searched.
     * @return The first declared constructor of the specified class.
     */
    protected static Constructor<?> getFirstDeclaredConstructorFor(Class<?> javaClass) {
        Constructor<?>[] constructors = javaClass.getDeclaredConstructors();
        return constructors[0];
    }

    /**
     * Checks if the specified field was declared static in its class.
     *
     * @param f The field to examine.
     * @return True when the Field is static.
     */
    protected static boolean isFieldStatic(final Field f) {
        return Modifier.isStatic(f.getModifiers());
    }

    protected static void logConstructorAccessException(Constructor<?> constructor, Exception e) {
        LOG.warning("Exception caught when invoking constructor " + constructor + ". Exception: " + e);
    }

    protected static void logPrivilegedConstructorAccessException(Constructor<?> constructor,
                                                                  PrivilegedActionException e) {
        LOG.warning("Exception caught on privileged invocation of constructor " + constructor + ". Exception: " + e);
    }
}
