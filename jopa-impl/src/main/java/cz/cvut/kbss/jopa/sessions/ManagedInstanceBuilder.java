package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.api.CloneConfiguration;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;

import java.lang.reflect.Field;

/**
 * Builds instances of entity types.
 * <p>
 * This builder expects the original's class has a public no-arg constructor. Furthermore, if the configuration
 * specifies that the result will be registered in a persistence context, the instance built is not the base Java type
 * of the original, but rather the {@link IdentifiableEntityType#getInstantiableJavaType()} result, which is a generated
 * subclass whose instances can be attached to the persistence context.
 */
public class ManagedInstanceBuilder extends DefaultInstanceBuilder {

    ManagedInstanceBuilder(CloneBuilderImpl builder, UnitOfWorkImpl uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration config) {
        assert uow.isEntityType(original.getClass());
        final IdentifiableEntityType<?> et = uow.getMetamodel().entity(original.getClass());
        final Class<?> cls = config.isForPersistenceContext() ? et.getInstantiableJavaType() : et.getJavaType();
        return ReflectionUtils.instantiateUsingDefaultConstructor(cls);
    }
}
