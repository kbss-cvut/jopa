/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

@SuppressWarnings("unused")
public class EntityLifecycleCallbackResolverTest {

    @Test
    public void resolveDiscoversEntityLifecycleListeners() throws Exception {
        final AbstractIdentifiableType<OWLClassR> type = typeFor(OWLClassR.class);
        final EntityLifecycleListenerManager callbackManager = resolve(type);
        assertTrue(callbackManager.getLifecycleCallbacks().containsKey(LifecycleEvent.POST_LOAD));
        assertEquals(OWLClassR.class.getDeclaredMethod("postLoad"),
                callbackManager.getLifecycleCallbacks().get(LifecycleEvent.POST_LOAD));
    }

    private EntityLifecycleListenerManager resolve(AbstractIdentifiableType<?> et) {
        return new EntityLifecycleCallbackResolver(et).resolve();
    }

    private <T> AbstractIdentifiableType<T> typeFor(Class<T> cls) {
        final String iri = cls.getDeclaredAnnotation(OWLClass.class).iri();
        return new ConcreteEntityType<>(cls, cls, IRI.create(iri));
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenMultipleListenersForOnePhaseAreDeclaredOnType() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(typeFor(ClassWithMultipleListeners.class)));
        assertEquals("The type [" + ClassWithMultipleListeners.class.getName() +
                "] has multiple lifecycle callbacks for the lifecycle event [" + LifecycleEvent.POST_LOAD +
                "].", ex.getMessage());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithMultipleListeners")
    public static class ClassWithMultipleListeners {
        @PostLoad
        private void one() {
        }

        @PostLoad
        private void two() {
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleListenerTakesArguments() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(typeFor(ClassWithLifecycleListenerTakingArguments.class)));
        assertEquals("The callback method [takesArguments] in type [" +
                ClassWithLifecycleListenerTakingArguments.class.getName() +
                "] has incorrect signature. It should not have any arguments.", ex.getMessage());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithLifecycleListenerTakingArguments")
    public static class ClassWithLifecycleListenerTakingArguments {

        @PrePersist
        private void takesArguments(EntityManager em) {
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleIsNotVoid() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(typeFor(ClassWithNonVoidLifecycleListener.class)));
        assertEquals("The callback method [returnsData] in type [" +
                ClassWithNonVoidLifecycleListener.class.getName() +
                "] has incorrect signature. Its return type should be void.", ex.getMessage());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithNonVoidLifecycleListener")
    public static class ClassWithNonVoidLifecycleListener {

        @PostPersist
        private String returnsData() {
            return "test";
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleListenerIsFinal() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(typeFor(ClassWithFinalLifecycleListener.class)));
        assertEquals("The callback method [finalListener] in type [" +
                ClassWithFinalLifecycleListener.class.getName() +
                "] has incorrect signature. It should not be static or final.", ex.getMessage());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithFinalLifecycleListener")
    public static class ClassWithFinalLifecycleListener {

        @PostPersist
        public final void finalListener() {
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleListenerIsStatic() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(typeFor(ClassWithStaticLifecycleListener.class)));
        assertEquals("The callback method [staticListener] in type [" +
                ClassWithStaticLifecycleListener.class.getName() +
                "] has incorrect signature. It should not be static or final.", ex.getMessage());
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithStaticLifecycleListener")
    public static class ClassWithStaticLifecycleListener {

        @PostPersist
        public static void staticListener() {
        }
    }

    @Test
    public void resolveSetsUpReferenceToSupertypeLifecycleManager() throws Exception {
        final AbstractIdentifiableType<OWLClassS> parent = typeFor(OWLClassS.class);
        parent.setLifecycleListenerManager(resolve(parent));
        final AbstractIdentifiableType<OWLClassR> child = typeFor(OWLClassR.class);
        child.setSupertypes(Collections.singleton(parent));
        final EntityLifecycleListenerManager result = resolve(child);
        final Field parentsField = EntityLifecycleListenerManager.class.getDeclaredField("parents");
        parentsField.setAccessible(true);

        Set<?> parentsFieldValue = (Set<?>) parentsField.get(result);
        assertEquals(1, parentsFieldValue.size());
        assertTrue(parentsFieldValue.contains(parent.getLifecycleListenerManager()));
    }

    @Test
    public void resolveCreatesInstanceOfEntityListenerDeclaredBySpecifiedEntityType() {
        final AbstractIdentifiableType<OWLClassS> et = typeFor(OWLClassS.class);
        final EntityLifecycleListenerManager result = resolve(et);
        assertEquals(1, result.getEntityListeners().size());
        assertInstanceOf(ParentListener.class, result.getEntityListeners().get(0));
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenUnableToInstantiateEntityListener() {
        final AbstractIdentifiableType<EntityWithInvalidListener> et = typeFor(EntityWithInvalidListener.class);
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(et));
        assertEquals("Unable to instantiate entity listener of type " + InvalidListener.class
                + ". The listener has to have a public no-arg constructor.", ex.getMessage());
    }

    public static class InvalidListener {
        private InvalidListener() {
        }
    }

    @TestLocal
    @EntityListeners(InvalidListener.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithInvalidListener")
    public static class EntityWithInvalidListener {
    }

    @Test
    public void resolveRegistersCallbacksDeclaredInEntityListener() throws Exception {
        final AbstractIdentifiableType<OWLClassS> et = typeFor(OWLClassS.class);
        final EntityLifecycleListenerManager result = resolve(et);
        assertEquals(1, result.getEntityListenerCallbacks().size());
        final Map<LifecycleEvent, Method> callbacks = result.getEntityListenerCallbacks().values().iterator().next();
        assertTrue(callbacks.containsKey(LifecycleEvent.PRE_PERSIST));
        assertEquals(ParentListener.getPrePersistMethod(), callbacks.get(LifecycleEvent.PRE_PERSIST));
        assertTrue(callbacks.containsKey(LifecycleEvent.POST_PERSIST));
        assertEquals(ParentListener.getPostPersistMethod(), callbacks.get(LifecycleEvent.POST_PERSIST));
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackHasInvalidNumberOfParameters() {
        final AbstractIdentifiableType<EntityWithListenerWithInvalidArgumentCount> et = typeFor(
                EntityWithListenerWithInvalidArgumentCount.class);
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(et));
        assertEquals("The callback method [invalidArgumentCount] in entity listener [" +
                ListenerWithInvalidArgumentCount.class.getName() +
                "] has incorrect signature. It should take exactly one argument.", ex.getMessage());
    }

    public static class ListenerWithInvalidArgumentCount {
        public ListenerWithInvalidArgumentCount() {
        }

        @PostLoad
        public void invalidArgumentCount(Object one, Object two) {
        }
    }

    @TestLocal
    @EntityListeners(ListenerWithInvalidArgumentCount.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithInvalidArgumentCount")
    public static class EntityWithListenerWithInvalidArgumentCount {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackIsNotVoid() {
        final AbstractIdentifiableType<EntityWithListenerWithNonVoidCallback> et = typeFor(
                EntityWithListenerWithNonVoidCallback.class);
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(et));
        assertEquals("The callback method [nonVoid] in entity listener [" +
                ListenerWithNonVoidCallback.class.getName() +
                "] has incorrect signature. Its return type should be void.", ex.getMessage());
    }

    public static class ListenerWithNonVoidCallback {
        public ListenerWithNonVoidCallback() {
        }

        @PostLoad
        public int nonVoid(Object arg) {
            return 117;
        }
    }

    @TestLocal
    @EntityListeners(ListenerWithNonVoidCallback.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithNonVoidCallback")
    private static class EntityWithListenerWithNonVoidCallback {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackIsStatic() {
        final AbstractIdentifiableType<EntityWithListenerWithStaticCallback> et = typeFor(
                EntityWithListenerWithStaticCallback.class);
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(et));
        assertEquals("The callback method [staticCallback] in entity listener [" +
                ListenerWithStaticCallback.class.getName() +
                "] has incorrect signature. It should not be static or final.", ex.getMessage());
    }

    public static class ListenerWithStaticCallback {
        public ListenerWithStaticCallback() {
        }

        @PostLoad
        public static void staticCallback(Object arg) {
        }
    }

    @TestLocal
    @EntityListeners(ListenerWithStaticCallback.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithStaticCallback")
    public static class EntityWithListenerWithStaticCallback {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerHasMultipleCallbacksForSameEvent() {
        final AbstractIdentifiableType<EntityWithListenerWithMultipleConflictingCallbacks> et = typeFor(
                EntityWithListenerWithMultipleConflictingCallbacks.class);
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(et));
        assertEquals("The entity listener [" + ListenerWithMultipleCallbacksForSameEvent.class.getName() +
                        "] has multiple callbacks for the lifecycle event [" + LifecycleEvent.POST_LOAD + "].",
                ex.getMessage());
    }

    public static class ListenerWithMultipleCallbacksForSameEvent {
        public ListenerWithMultipleCallbacksForSameEvent() {
        }

        @PostLoad
        public void firstCallback(Object arg) {
        }

        @PostLoad
        public void secondCallback(Object arg) {
        }
    }

    @TestLocal
    @EntityListeners(ListenerWithMultipleCallbacksForSameEvent.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithMultipleConflictingCallbacks")
    public static class EntityWithListenerWithMultipleConflictingCallbacks {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackHasInvalidParameterType() {
        final AbstractIdentifiableType<EntityWithListenerWithInvalidParameterTypeCallback> et = typeFor(
                EntityWithListenerWithInvalidParameterTypeCallback.class);
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> resolve(et));
        assertEquals("The callback method [callback] in entity listener [" +
                ListenerWithInvalidParameterTypeCallback.class.getName() +
                "] has incorrect signature. Its parameter should be of type [" + Object.class.getName() + "] or [" +
                EntityWithListenerWithInvalidParameterTypeCallback.class.getName() + "].", ex.getMessage());
    }

    public static class ListenerWithInvalidParameterTypeCallback {
        public ListenerWithInvalidParameterTypeCallback() {
        }

        @PostLoad
        public void callback(OWLClassA arg) {
        }
    }

    @TestLocal
    @EntityListeners(ListenerWithInvalidParameterTypeCallback.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithInvalidParameterTypeCallback")
    public static class EntityWithListenerWithInvalidParameterTypeCallback {
    }
}
