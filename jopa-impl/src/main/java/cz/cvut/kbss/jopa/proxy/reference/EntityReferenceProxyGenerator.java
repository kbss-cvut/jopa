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
package cz.cvut.kbss.jopa.proxy.reference;

import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistenceContextAwareClassGenerator;
import cz.cvut.kbss.jopa.model.metamodel.gen.PersistentPropertySetterMatcher;
import cz.cvut.kbss.jopa.proxy.lazy.gen.PersistentPropertyGetterMatcher;
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
import java.net.URI;
import java.util.Objects;

import static net.bytebuddy.matcher.ElementMatchers.isGetter;
import static net.bytebuddy.matcher.ElementMatchers.isSetter;

public class EntityReferenceProxyGenerator implements PersistenceContextAwareClassGenerator {

    private static final Logger LOG = LoggerFactory.getLogger(EntityReferenceProxyGenerator.class);

    private final ByteBuddy byteBuddy = new ByteBuddy().with(new NamingStrategy.AbstractBase() {
        @Override
        protected String name(TypeDescription typeDescription) {
            return typeDescription.getSimpleName() + "_ReferenceProxy";
        }
    });

    @Override
    public <T> Class<? extends T> generate(Class<T> entityClass) {
        Objects.requireNonNull(entityClass);
        LOG.trace("Generating reference proxy for entity class {}.", entityClass);
        DynamicType.Unloaded<? extends T> typeDef = byteBuddy.subclass(entityClass)
                                                             .annotateType(new GeneratedEntityReferenceProxyImpl())
                                                             .defineField("identifier", URI.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .defineField("type", Class.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .defineField("descriptor", Descriptor.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .defineField("persistenceContext", UnitOfWork.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             // Have to use Object, because otherwise it won't generate a setter for us
                                                             .defineField("value", entityClass, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .implement(TypeDescription.Generic.Builder.parameterizedType(EntityReferenceProxyPropertyAccessor.class, entityClass)
                                                                                                       .build())
                                                             .intercept(FieldAccessor.ofBeanProperty())
                                                             .implement(EntityReferenceProxy.class)
                                                             .method(isSetter().and(new PersistentPropertySetterMatcher<>(entityClass)))
                                                             .intercept(MethodDelegation.to(SetterInterceptor.class))
                                                             .method(isGetter().and(new PersistentPropertyGetterMatcher<>(entityClass)))
                                                             .intercept(MethodDelegation.to(GetterInterceptor.class))
                                                             .make();
        LOG.debug("Generated dynamic type {} for entity class {}.", typeDef, entityClass);
        return typeDef.load(getClass().getClassLoader()).getLoaded();
    }

    public static class GetterInterceptor {

        private GetterInterceptor() {
            throw new AssertionError();
        }

        @RuntimeType
        public static <T> Object get(@This EntityReferenceProxy<T> proxy, @Origin Method getter) {
            if (isIdentifierField(proxy, getter)) {
                return proxy.getIdentifier();
            }
            final Object loaded = proxy.triggerLoading();
            try {
                return getter.invoke(loaded);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new LazyLoadingException("Unable to invoke getter after loading referenced object.", e);
            }
        }
    }

    private static <T> boolean isIdentifierField(EntityReferenceProxy<T> proxy, Method accessor) {
        final String fieldName = AnnotatedAccessor.from(accessor).getPropertyName();
        return proxy.getPersistenceContext().getMetamodel().entity(proxy.getType()).getIdentifier().getName()
                    .equals(fieldName);
    }

    public static class SetterInterceptor {

        private SetterInterceptor() {
            throw new AssertionError();
        }

        public static <T> void set(@This EntityReferenceProxy<T> proxy, @Origin Method setter,
                                   @AllArguments Object[] args) {
            if (isIdentifierField(proxy, setter)) {
                throw new AttributeModificationForbiddenException("Cannot change entity reference identifier.");
            }
            final Object loaded = proxy.triggerLoading();
            try {
                setter.invoke(loaded, args);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new LazyLoadingException("Unable to invoke setter after loading referenced object.", e);
            }
        }
    }
}
