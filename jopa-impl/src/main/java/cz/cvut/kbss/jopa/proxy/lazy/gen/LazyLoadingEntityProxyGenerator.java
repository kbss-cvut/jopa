package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistenceContextAwareClassGenerator;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistentPropertySetterMatcher;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.NamingStrategy;
import net.bytebuddy.description.modifier.FieldPersistence;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.implementation.FieldAccessor;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.bind.annotation.AllArguments;
import net.bytebuddy.implementation.bind.annotation.Origin;
import net.bytebuddy.implementation.bind.annotation.RuntimeType;
import net.bytebuddy.implementation.bind.annotation.This;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Objects;

import static net.bytebuddy.matcher.ElementMatchers.isGetter;
import static net.bytebuddy.matcher.ElementMatchers.isSetter;
import static net.bytebuddy.matcher.ElementMatchers.isToString;

public class LazyLoadingEntityProxyGenerator implements PersistenceContextAwareClassGenerator {

    private static final Logger LOG = LoggerFactory.getLogger(LazyLoadingEntityProxyGenerator.class);

    private final ByteBuddy byteBuddy = new ByteBuddy().with(new NamingStrategy.AbstractBase() {
        @Override
        protected String name(TypeDescription typeDescription) {
            return typeDescription.getSimpleName() + "_LazyLoadingProxy";
        }
    });

    @Override
    public <T> Class<? extends T> generate(Class<T> entityClass) {
        Objects.requireNonNull(entityClass);
        LOG.trace("Generating lazy loading proxy for entity class {}.", entityClass);
        DynamicType.Unloaded<? extends T> typeDef = byteBuddy.subclass(entityClass)
                                                             .annotateType(new GeneratedLazyLoadingProxyImpl())
                                                             .defineField("persistenceContext", UnitOfWork.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .defineField("owner", Object.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .defineField("fieldSpec", FieldSpecification.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .implement(LazyLoadingProxyPropertyAccessor.class)
                                                             .intercept(FieldAccessor.ofBeanProperty())
                                                             .implement(LazyLoadingEntityProxy.class)
                                                             .method(isSetter().and(new PersistentPropertySetterMatcher<>(entityClass)))
                                                             .intercept(MethodDelegation.to(SetterInterceptor.class))
                                                             .method(isGetter().and(new PersistentPropertyGetterMatcher<>(entityClass)))
                                                             .intercept(MethodDelegation.to(GetterInterceptor.class))
                                                             .method(isToString())
                                                             .intercept(MethodDelegation.toMethodReturnOf("stringify"))
                                                             .make();
        LOG.debug("Generated dynamic type {} for entity class {}.", typeDef, entityClass);
        return typeDef.load(getClass().getClassLoader()).getLoaded();
    }

    public static class GetterInterceptor {

        @RuntimeType
        public static <T> Object get(@This LazyLoadingEntityProxy<T> proxy, @Origin Method getter) {
            final Object loaded = proxy.triggerLazyLoading();
            try {
                return getter.invoke(loaded);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new LazyLoadingException("Unable to invoke getter after lazily loading object.", e);
            }
        }
    }

    public static class SetterInterceptor {

        public static <T> void set(@This LazyLoadingEntityProxy<T> proxy, @Origin Method setter,
                                   @AllArguments Object[] args) {
            final Object loaded = proxy.triggerLazyLoading();
            try {
                setter.invoke(loaded, args);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new LazyLoadingException("Unable to invoke setter after lazily loading object.", e);
            }
        }
    }
}
