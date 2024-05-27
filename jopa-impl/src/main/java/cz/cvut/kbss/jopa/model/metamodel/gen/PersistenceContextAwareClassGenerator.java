package cz.cvut.kbss.jopa.model.metamodel.gen;

/**
 * Generates classes corresponding to entity classes, but able to be connected to a persistence context.
 */
public interface PersistenceContextAwareClassGenerator {

    /**
     * Generates a class extending the specified {@code entityClass} but able to be connected to a persistence context.
     *
     * @param entityClass Entity class for which to generate the subclass
     * @param <T>         Entity type
     * @return Entity class subtype
     */
    <T> Class<? extends T> generate(Class<T> entityClass);
}
