/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.MappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.Namespace;
import cz.cvut.kbss.jopa.model.annotations.Namespaces;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;

import java.lang.reflect.AnnotatedElement;

/**
 * Utility methods for processing managed types for metamodel construction.
 */
class ManagedClassProcessor {

    private static final EntityLifecycleCallbackResolver lifecycleCallbackResolver =
            new EntityLifecycleCallbackResolver();

    private ManagedClassProcessor() {
    }

    static <T> TypeBuilderContext<T> processManagedType(Class<T> cls) {
        final NamespaceResolver resolver = detectNamespaces(cls);
        final AbstractIdentifiableType<T> type;
        if (isEntityType(cls)) {
            type = processEntityType(cls, resolver);
        } else if (isMappedSuperclassType(cls)) {
            type = processMappedSuperclassType(cls);
        } else {
            throw new MetamodelInitializationException("Type " + cls + " is not a managed type.");
        }
        final EntityLifecycleListenerManager callbackManager = lifecycleCallbackResolver.resolve(type);
        type.setLifecycleListenerManager(callbackManager);
        return new TypeBuilderContext<>(type, resolver);
    }

    private static <T> NamespaceResolver detectNamespaces(Class<T> cls) {
        final NamespaceResolver resolver = new NamespaceResolver();
        if (cls.getPackage() != null) {
            resolveNamespaces(cls.getPackage(), resolver);
        }
        resolveNamespaces(cls, resolver);
        return resolver;
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

    private static <T> EntityTypeImpl<T> processEntityType(Class<T> cls, NamespaceResolver namespaceResolver) {
        final OWLClass c = cls.getDeclaredAnnotation(OWLClass.class);
        assert c != null;

        checkForNoArgConstructor(cls);

        return new EntityTypeImpl<>(cls.getSimpleName(), cls, IRI.create(namespaceResolver.resolveFullIri(c.iri())));
    }

    private static <T> void checkForNoArgConstructor(Class<T> cls) {
        try {
            cls.getDeclaredConstructor();
        } catch (NoSuchMethodException e) {
            throw new MetamodelInitializationException("Class " + cls + " is missing required no-arg constructor.", e);
        }
    }

    private static <T> MappedSuperclassTypeImpl<T> processMappedSuperclassType(Class<T> cls) {
        assert cls.getDeclaredAnnotation(MappedSuperclass.class) != null;

        return new MappedSuperclassTypeImpl<>(cls);
    }

    static <T> Class<? super T> getManagedSupertype(Class<T> cls) {
        if (cls.getSuperclass() != null && isManagedType(cls.getSuperclass())) {
            return cls.getSuperclass();
        }
        return null;
    }

    private static boolean isManagedType(Class<?> cls) {
        return isEntityType(cls) || isMappedSuperclassType(cls);
    }

    private static boolean isEntityType(Class<?> cls) {
        return cls.getDeclaredAnnotation(OWLClass.class) != null;
    }

    private static boolean isMappedSuperclassType(Class<?> cls) {
        return cls.getDeclaredAnnotation(MappedSuperclass.class) != null;
    }
}
