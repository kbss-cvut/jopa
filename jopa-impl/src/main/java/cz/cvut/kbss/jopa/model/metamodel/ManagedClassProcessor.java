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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.Namespace;
import cz.cvut.kbss.jopa.model.annotations.Namespaces;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.gen.ManageableClassGenerator;
import cz.cvut.kbss.jopa.utils.ChangeTrackingMode;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Utility methods for processing managed types for metamodel construction.
 */
public class ManagedClassProcessor {

    private ManagedClassProcessor() {
        throw new AssertionError();
    }

    static <T> TypeBuilderContext<T> processManagedType(Class<T> cls, NamespaceResolver namespaceResolver,
                                                        Configuration config) {
        final AbstractIdentifiableType<T> type;
        if (isEntityType(cls)) {
            type = processEntityType(cls, namespaceResolver, config);
        } else if (isMappedSuperclassType(cls)) {
            type = processMappedSuperclassType(cls);
        } else {
            throw new MetamodelInitializationException("Type " + cls + " is not a managed type.");
        }
        return new TypeBuilderContext<>(type, namespaceResolver);
    }

    /**
     * Detects namespace declarations relevant to the specified class and registers them with the specified
     * {@link NamespaceResolver}.
     * <p>
     * This means namespaces declared on the class itself, namespaces it inherited from super types, as well as
     * namespaces declared for the package that contains the specified class.
     * <p>
     * Namespaces declared directly by {@link Namespace} as well as in {@link Namespaces} are considered.
     *
     * @param cls               Class to detect namespaces for
     * @param namespaceResolver Namespace resolver containing detected namespaces
     */
    public static <T> void detectNamespaces(Class<T> cls, NamespaceResolver namespaceResolver) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(namespaceResolver);
        if (cls.getPackage() != null) {
            resolveNamespaces(cls.getPackage(), namespaceResolver);
        }
        resolveNamespaces(cls, namespaceResolver);
    }

    private static void resolveNamespaces(AnnotatedElement target, NamespaceResolver namespaceResolver) {
        final Namespaces namespaces = target.getDeclaredAnnotation(Namespaces.class);
        if (namespaces != null) {
            for (Namespace ns : namespaces.value()) {
                namespaceResolver.registerNamespace(ns.prefix(), ns.namespace());
            }
        }
        final Namespace namespace = target.getDeclaredAnnotation(Namespace.class);
        if (namespace != null) {
            namespaceResolver.registerNamespace(namespace.prefix(), namespace.namespace());
        }
    }

    private static <T> IdentifiableEntityType<T> processEntityType(Class<T> cls, NamespaceResolver namespaceResolver,
                                                                   Configuration config) {
        final OWLClass c = cls.getDeclaredAnnotation(OWLClass.class);
        assert c != null;

        if (cls.isInterface() || Modifier.isAbstract(cls.getModifiers())) {
            return new AbstractEntityType<>(cls, IRI.create(namespaceResolver.resolveFullIri(c.iri())));
        } else {
            checkForNoArgConstructor(cls);
            final Class<? extends T> instantiableType = resolveInstantiableType(cls, config);
            return new ConcreteEntityType<>(cls, instantiableType, IRI.create(namespaceResolver.resolveFullIri(c.iri())));
        }
    }

    private static <T> Class<? extends T> resolveInstantiableType(Class<T> cls, Configuration config) {
        if (ChangeTrackingMode.IMMEDIATE == ChangeTrackingMode.resolve(config)) {
            return new ManageableClassGenerator(config).generate(cls);
        } else {
            return cls;
        }
    }

    private static <T> void checkForNoArgConstructor(Class<T> cls) {
        try {
            cls.getDeclaredConstructor();
        } catch (NoSuchMethodException e) {
            throw new MetamodelInitializationException("Entity " + cls + " is missing required no-arg constructor.", e);
        }
    }

    private static <T> MappedSuperclassTypeImpl<T> processMappedSuperclassType(Class<T> cls) {
        assert cls.getDeclaredAnnotation(MappedSuperclass.class) != null;

        return new MappedSuperclassTypeImpl<>(cls);
    }

    static <T> Class<? super T> getManagedSuperClass(Class<T> cls) {
        if (cls.getSuperclass() != null && isManagedType(cls.getSuperclass())) {
            return cls.getSuperclass();
        }
        return null;
    }

    static <T> Set<Class<? super T>> getManagedSuperInterfaces(Class<T> cls) {
        return Arrays.stream(cls.getInterfaces()).filter(ManagedClassProcessor::isManagedType)
                     .map(clazz -> (Class<? super T>) clazz)
                     .collect(Collectors.toSet());
    }

    /**
     * Checks whether the specified class is a managed type.
     * <p>
     * That is, if it is an entity type (annotated with {@link OWLClass}) or a mapped superclass (annotated with
     * {@link MappedSuperclass}).
     *
     * @param cls Class to check
     * @return {@code true} if the class is a managed type, {@code false} otherwise
     */
    public static boolean isManagedType(Class<?> cls) {
        return isEntityType(cls) || isMappedSuperclassType(cls);
    }

    /**
     * Checks whether the specified class is an entity type.
     * <p>
     * An entity is annotated with {@link OWLClass}.
     *
     * @param cls Class to check
     * @return {@code true} if the class is an entity type, {@code false} otherwise
     */
    public static boolean isEntityType(Class<?> cls) {
        return cls != null && cls.getDeclaredAnnotation(OWLClass.class) != null;
    }

    /**
     * Checks whether the specified class is a managed superclass.
     * <p>
     * An entity is annotated with {@link MappedSuperclass}.
     *
     * @param cls Class to check
     * @return {@code true} if the class is a mapped superclass, {@code false} otherwise
     */
    public static boolean isMappedSuperclassType(Class<?> cls) {
        return cls != null && cls.getDeclaredAnnotation(MappedSuperclass.class) != null;
    }
}
