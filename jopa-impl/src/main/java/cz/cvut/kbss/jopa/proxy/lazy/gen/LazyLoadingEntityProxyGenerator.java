/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.proxy.lazy.gen;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistenceContextAwareClassGenerator;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistentPropertySetterMatcher;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.NamingStrategy;
import net.bytebuddy.description.modifier.FieldPersistence;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.implementation.FieldAccessor;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.bind.annotation.AllArguments;
import net.bytebuddy.implementation.bind.annotation.Argument;
import net.bytebuddy.implementation.bind.annotation.FieldValue;
import net.bytebuddy.implementation.bind.annotation.Origin;
import net.bytebuddy.implementation.bind.annotation.RuntimeType;
import net.bytebuddy.implementation.bind.annotation.This;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Objects;

import static net.bytebuddy.matcher.ElementMatchers.isEquals;
import static net.bytebuddy.matcher.ElementMatchers.isGetter;
import static net.bytebuddy.matcher.ElementMatchers.isHashCode;
import static net.bytebuddy.matcher.ElementMatchers.isSetter;
import static net.bytebuddy.matcher.ElementMatchers.isToString;
import static net.bytebuddy.matcher.ElementMatchers.named;

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
        DynamicType.Builder<T> builder = byteBuddy.subclass(entityClass)
                                                  .annotateType(new GeneratedLazyLoadingProxyImpl())
                                                  .defineField("persistenceContext", UnitOfWork.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                  .defineField("owner", Object.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                  .defineField("fieldSpec", FieldSpecification.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                  // Have to use Object, because otherwise it won't generate a setter for us
                                                  .defineField("value", entityClass, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                  .implement(TypeDescription.Generic.Builder.parameterizedType(LazyLoadingProxyPropertyAccessor.class, entityClass)
                                                                                            .build())
                                                  .intercept(FieldAccessor.ofBeanProperty())
                                                  .implement(LazyLoadingEntityProxy.class)
                                                  .method(isSetter().and(new PersistentPropertySetterMatcher<>(entityClass)))
                                                  .intercept(MethodDelegation.to(SetterInterceptor.class))
                                                  .method(isGetter().and(new PersistentPropertyGetterMatcher<>(entityClass)))
                                                  .intercept(MethodDelegation.to(GetterInterceptor.class))
                                                  .method(isToString())
                                                  .intercept(MethodDelegation.toMethodReturnOf("stringify"))
                                                  .method(named("isLoaded"))
                                                  .intercept(MethodDelegation.to(ProxyMethodsInterceptor.class))
                                                  .method(named("getLoadedValue"))
                                                  .intercept(MethodDelegation.to(ProxyMethodsInterceptor.class));
        if (ReflectionUtils.overridesEquals(entityClass)) {
            builder = builder.method(isEquals()).intercept(MethodDelegation.to(EqualsInterceptor.class));
        }
        if (ReflectionUtils.overridesHashCode(entityClass)) {
            builder = builder.method(isHashCode()).intercept(MethodDelegation.to(GetterInterceptor.class));
        }
        final DynamicType.Unloaded<? extends T> typeDef = builder.make();
        LOG.debug("Generated dynamic type {} for entity class {}.", typeDef, entityClass);
        return typeDef.load(getClass().getClassLoader()).getLoaded();
    }

    public static class GetterInterceptor {

        private GetterInterceptor() {
            throw new AssertionError();
        }

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

    public static class EqualsInterceptor {
        private EqualsInterceptor() {
            throw new AssertionError();
        }

        public static <T> boolean equals(@This LazyLoadingEntityProxy<T> proxy, @Origin Method equals,
                                         @Argument(0) Object arg) {
            final Object loaded = proxy.triggerLazyLoading();
            try {
                return (boolean) equals.invoke(loaded, arg);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new LazyLoadingException("Unable to invoke getter after lazily loading object.", e);
            }
        }
    }

    public static class SetterInterceptor {

        private SetterInterceptor() {
            throw new AssertionError();
        }

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

    public static class ProxyMethodsInterceptor {

        private ProxyMethodsInterceptor() {
            throw new AssertionError();
        }

        public static <T> boolean isLoaded(@This LazyLoadingEntityProxy<T> proxy, @FieldValue("value") T value) {
            return value != null;
        }

        public static <T> T getLoadedValue(@This LazyLoadingEntityProxy<T> proxy, @FieldValue("value") T value) {
            if (value == null) {
                throw new IllegalStateException("Proxy has not been loaded, yet.");
            }
            return value;
        }
    }
}
