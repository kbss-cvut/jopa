package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.description.modifier.FieldPersistence;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.implementation.FieldAccessor;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.SuperMethodCall;
import net.bytebuddy.implementation.bind.annotation.Origin;
import net.bytebuddy.implementation.bind.annotation.This;

import java.lang.reflect.Method;
import java.util.Objects;

import static net.bytebuddy.matcher.ElementMatchers.isGetter;
import static net.bytebuddy.matcher.ElementMatchers.isSetter;
import static net.bytebuddy.matcher.ElementMatchers.named;
import static net.bytebuddy.matcher.ElementMatchers.not;

/**
 * Generates persistence context-aware classes that implement the {@link cz.cvut.kbss.jopa.model.Manageable} interface.
 * <p>
 * Such classes have an additional attribute not inherited from the base entity class. This attribute's value is a
 * reference to the persistence context to which an instance is attached. {@link cz.cvut.kbss.jopa.model.Manageable}
 * allows establishing and accessing this connection.
 */
public class ManageableClassGenerator implements PersistenceContextAwareClassGenerator {

    private final ByteBuddy byteBuddy = new ByteBuddy().with(new ByteBuddyUtil.NameGenerator());

    @Override
    public <T> Class<? extends T> generate(Class<T> entityClass) {
        Objects.requireNonNull(entityClass);
        DynamicType.Unloaded<? extends T> typeDef = byteBuddy.subclass(entityClass)
                                                             .annotateType(entityClass.getAnnotations())
                                                             .annotateType(new GeneratedEntityClassImpl())
                                                             .defineField("persistenceContext", UnitOfWorkImpl.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .implement(Manageable.class)
                                                             .intercept(FieldAccessor.ofBeanProperty())
                                                             .method(isSetter().and(not(named("setPersistenceContext"))))
                                                             .intercept(SuperMethodCall.INSTANCE.andThen(MethodDelegation.to(SetterInterceptor.class)))
                                                             .method(isGetter().and(not(named("getPersistenceContext"))))
                                                             .intercept(MethodDelegation.to(GetterInterceptor.class)
                                                                                        .andThen(SuperMethodCall.INSTANCE))
                                                             .make();

        return typeDef.load(getClass().getClassLoader()).getLoaded();
    }

    public static class SetterInterceptor {

        public static void set(@This Manageable instance, @Origin Method setter) throws Exception {
            final UnitOfWorkImpl pc = instance.getPersistenceContext();
            if (pc == null || !pc.isInTransaction()) {
                return;
            }
            final String fieldName = AnnotatedAccessor.from(setter).getPropertyName();
            final EntityType<?> et = pc.getMetamodel().entity(MetamodelUtils.getEntityClass(instance.getClass()));
            final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(fieldName);
            if (EntityPropertiesUtils.isFieldTransient(fieldSpec.getJavaField())) {
                return;
            }
            AttributeModificationValidator.verifyCanModify(fieldSpec);
            pc.attributeChanged(instance, fieldSpec);
        }
    }

    public static class GetterInterceptor {

        public static void get(@This Manageable instance, @Origin Method getter) throws Exception {
            final UnitOfWorkImpl pc = instance.getPersistenceContext();
            if (pc == null || !pc.contains(instance)) {
                return;
            }
            final String fieldName = AnnotatedAccessor.from(getter).getPropertyName();
            final EntityType<?> et = pc.getMetamodel().entity(MetamodelUtils.getEntityClass(instance.getClass()));
            final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(fieldName);
            if (EntityPropertiesUtils.isFieldTransient(fieldSpec.getJavaField()) || et.getIdentifier()
                                                                                      .equals(fieldSpec)) {
                return;
            }
            pc.loadEntityField(instance, (FieldSpecification) fieldSpec);
        }
    }
}
