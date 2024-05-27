package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.metamodel.gen.PersistenceContextAwareClassGenerator;

/**
 * Returns the specified entity class.
 */
public class NoopInstantiableTypeGenerator implements PersistenceContextAwareClassGenerator {

    public static final NoopInstantiableTypeGenerator INSTANCE = new NoopInstantiableTypeGenerator();

    private NoopInstantiableTypeGenerator() {
    }

    @Override
    public <T> Class<? extends T> generate(Class<T> entityClass) {
        return entityClass;
    }
}
