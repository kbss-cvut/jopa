/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
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
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * Utility methods for processing managed types for metamodel construction.
 */
class ManagedClassProcessor {

    private ManagedClassProcessor() {
    }

    static <T> AbstractIdentifiableType<T> processManagedType(Class<T> cls) {
        final AbstractIdentifiableType<T> type;
        if (isEntityType(cls)) {
            type = processEntityType(cls);
        } else if (isMappedSuperclassType(cls)) {
            type = processMappedSuperclassType(cls);
        } else {
            throw new MetamodelInitializationException("Type " + cls + " is not a managed type.");
        }
        resolveLifecycleHooks(type);
        return type;
    }

    private static <T> EntityTypeImpl<T> processEntityType(Class<T> cls) {
        final OWLClass c = cls.getDeclaredAnnotation(OWLClass.class);
        assert c != null;

        checkForNoArgConstructor(cls);

        return new EntityTypeImpl<>(cls.getSimpleName(), cls, IRI.create(c.iri()));
    }

    private static <T> void checkForNoArgConstructor(Class<T> cls) {
        try {
            cls.getDeclaredConstructor();
        } catch (NoSuchMethodException e) {
            throw new MetamodelInitializationException("Class " + cls + " is missing required no-arg constructor.");
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

    private static <T> void resolveLifecycleHooks(AbstractIdentifiableType<T> type) {
        final Class<T> cls = type.getJavaType();
        for (Method m : cls.getDeclaredMethods()) {
            for (LifecycleEvent hookType : LifecycleEvent.values()) {
                if (m.getDeclaredAnnotation(hookType.getAnnotation()) != null) {
                    verifyCallbackNotAlreadyDefined(type, hookType);
                    verifyListenerSignature(type, m);
                    type.addLifecycleListener(hookType, m);
                }
            }
        }
    }

    private static <T> void verifyCallbackNotAlreadyDefined(AbstractIdentifiableType<T> type, LifecycleEvent hookType) {
        if (type.hasDeclaredLifecycleListener(hookType)) {
            throw MetamodelInitializationException.multipleListenersForSameLifecycleEvent(type.getJavaType(), hookType);
        }
    }

    private static <T> void verifyListenerSignature(AbstractIdentifiableType<T> type, Method listener) {
        if (listener.getParameterCount() > 0) {
            throw MetamodelInitializationException.invalidArgumentsForLifecycleListener(type.getJavaType(), listener);
        }
        if (!listener.getReturnType().equals(Void.TYPE)) {
            throw MetamodelInitializationException.invalidReturnTypeForLifecycleListener(type.getJavaType(), listener);
        }
        if (Modifier.isFinal(listener.getModifiers()) || Modifier.isStatic(listener.getModifiers())) {
            throw MetamodelInitializationException.invalidLifecycleListenerModifier(type.getJavaType(), listener);
        }
    }
}
