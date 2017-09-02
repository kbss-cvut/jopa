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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Map;

import static org.junit.Assert.*;

@SuppressWarnings("unused")
public class EntityLifecycleCallbackResolverTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private EntityLifecycleCallbackResolver resolver = new EntityLifecycleCallbackResolver();

    @Test
    public void resolveDiscoversEntityLifecycleListeners() throws Exception {
        final AbstractIdentifiableType<OWLClassR> type = typeFor(OWLClassR.class);
        final EntityLifecycleListenerManager callbackManager = resolver.resolve(type);
        assertTrue(callbackManager.getLifecycleCallbacks().containsKey(LifecycleEvent.POST_LOAD));
        assertEquals(OWLClassR.class.getDeclaredMethod("postLoad"),
                callbackManager.getLifecycleCallbacks().get(LifecycleEvent.POST_LOAD));
    }

    private <T> AbstractIdentifiableType<T> typeFor(Class<T> cls) {
        final String name = cls.getName();
        final String iri = cls.getDeclaredAnnotation(OWLClass.class).iri();
        return new EntityTypeImpl<>(name, cls, IRI.create(iri));
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenMultipleListenersForOnePhaseAreDeclaredOnType()
            throws Exception {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The type [" + ClassWithMultipleListeners.class.getName() +
                "] has multiple lifecycle callbacks for the lifecycle event [" + LifecycleEvent.POST_LOAD +
                "].");
        resolver.resolve(typeFor(ClassWithMultipleListeners.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithMultipleListeners")
    private static class ClassWithMultipleListeners {
        @PostLoad
        private void one() {
        }

        @PostLoad
        private void two() {
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleListenerTakesArguments() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [takesArguments] in type [" +
                ClassWithLifecycleListenerTakingArguments.class.getName() +
                "] has incorrect signature. It should not have any arguments.");
        resolver.resolve(typeFor(ClassWithLifecycleListenerTakingArguments.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithLifecycleListenerTakingArguments")
    private static class ClassWithLifecycleListenerTakingArguments {

        @PrePersist
        private void takesArguments(EntityManager em) {
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleIsNotVoid() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [returnsData] in type [" +
                ClassWithNonVoidLifecycleListener.class.getName() +
                "] has incorrect signature. Its return type should be void.");
        resolver.resolve(typeFor(ClassWithNonVoidLifecycleListener.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithNonVoidLifecycleListener")
    private static class ClassWithNonVoidLifecycleListener {

        @PostPersist
        private String returnsData() {
            return "test";
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleListenerIsFinal() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [finalListener] in type [" +
                ClassWithFinalLifecycleListener.class.getName() +
                "] has incorrect signature. It should not be static or final.");
        resolver.resolve(typeFor(ClassWithFinalLifecycleListener.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithFinalLifecycleListener")
    private static class ClassWithFinalLifecycleListener {

        @PostPersist
        public final void finalListener() {
        }
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenLifecycleListenerIsStatic() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [staticListener] in type [" +
                ClassWithStaticLifecycleListener.class.getName() +
                "] has incorrect signature. It should not be static or final.");
        resolver.resolve(typeFor(ClassWithStaticLifecycleListener.class));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ClassWithStaticLifecycleListener")
    private static class ClassWithStaticLifecycleListener {

        @PostPersist
        public static void staticListener() {
        }
    }

    @Test
    public void resolveSetsUpReferenceToSupertypeLifecycleManager() throws Exception {
        final AbstractIdentifiableType<OWLClassS> parent = typeFor(OWLClassS.class);
        parent.setLifecycleListenerManager(resolver.resolve(parent));
        final AbstractIdentifiableType<OWLClassR> child = typeFor(OWLClassR.class);
        child.setSupertype(parent);
        final EntityLifecycleListenerManager result = resolver.resolve(child);
        final Field parentField = EntityLifecycleListenerManager.class.getDeclaredField("parent");
        parentField.setAccessible(true);
        assertEquals(parent.getLifecycleListenerManager(), parentField.get(result));
    }

    @Test
    public void resolveCreatesInstanceOfEntityListenerDeclaredBySpecifiedEntityType() throws Exception {
        final AbstractIdentifiableType<OWLClassS> et = typeFor(OWLClassS.class);
        final EntityLifecycleListenerManager result = resolver.resolve(et);
        assertEquals(1, result.getEntityListeners().size());
        assertTrue(result.getEntityListeners().get(0) instanceof ParentListener);
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenUnableToInstantiateEntityListener() throws Exception {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("Unable to instantiate entity listener of type " + InvalidListener.class
                + ". The listener has to have a public no-arg constructor.");
        final AbstractIdentifiableType<EntityWithInvalidListener> et = typeFor(EntityWithInvalidListener.class);
        resolver.resolve(et);
    }

    private static class InvalidListener {
        private InvalidListener() {
        }
    }

    @EntityListeners(InvalidListener.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithInvalidListener")
    private static class EntityWithInvalidListener {
    }

    @Test
    public void resolveRegistersCallbacksDeclaredInEntityListener() throws Exception {
        final AbstractIdentifiableType<OWLClassS> et = typeFor(OWLClassS.class);
        final EntityLifecycleListenerManager result = resolver.resolve(et);
        assertEquals(1, result.getEntityListenerCallbacks().size());
        final Map<LifecycleEvent, Method> callbacks = result.getEntityListenerCallbacks().values().iterator().next();
        assertTrue(callbacks.containsKey(LifecycleEvent.PRE_PERSIST));
        assertEquals(ParentListener.getPrePersistMethod(), callbacks.get(LifecycleEvent.PRE_PERSIST));
        assertTrue(callbacks.containsKey(LifecycleEvent.POST_PERSIST));
        assertEquals(ParentListener.getPostPersistMethod(), callbacks.get(LifecycleEvent.POST_PERSIST));
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackHasInvalidNumberOfParameters() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [invalidArgumentCount] in entity listener [" +
                ListenerWithInvalidArgumentCount.class.getName() +
                "] has incorrect signature. It should take exactly one argument.");
        final AbstractIdentifiableType<EntityWithListenerWithInvalidArgumentCount> et = typeFor(
                EntityWithListenerWithInvalidArgumentCount.class);
        resolver.resolve(et);
    }

    private static class ListenerWithInvalidArgumentCount {
        public ListenerWithInvalidArgumentCount() {
        }

        @PostLoad
        public void invalidArgumentCount(Object one, Object two) {
        }
    }

    @EntityListeners(ListenerWithInvalidArgumentCount.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithInvalidArgumentCount")
    private static class EntityWithListenerWithInvalidArgumentCount {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackIsNotVoid() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [nonVoid] in entity listener [" +
                ListenerWithNonVoidCallback.class.getName() +
                "] has incorrect signature. Its return type should be void.");
        final AbstractIdentifiableType<EntityWithListenerWithNonVoidCallback> et = typeFor(
                EntityWithListenerWithNonVoidCallback.class);
        resolver.resolve(et);
    }

    private static class ListenerWithNonVoidCallback {
        public ListenerWithNonVoidCallback() {
        }

        @PostLoad
        public int nonVoid(Object arg) {
            return 117;
        }
    }

    @EntityListeners(ListenerWithNonVoidCallback.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithNonVoidCallback")
    private static class EntityWithListenerWithNonVoidCallback {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackIsStatic() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [staticCallback] in entity listener [" +
                ListenerWithStaticCallback.class.getName() +
                "] has incorrect signature. It should not be static or final.");
        final AbstractIdentifiableType<EntityWithListenerWithStaticCallback> et = typeFor(
                EntityWithListenerWithStaticCallback.class);
        resolver.resolve(et);
    }

    private static class ListenerWithStaticCallback {
        public ListenerWithStaticCallback() {
        }

        @PostLoad
        public static void staticCallback(Object arg) {
        }
    }

    @EntityListeners(ListenerWithStaticCallback.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithStaticCallback")
    private static class EntityWithListenerWithStaticCallback {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerHasMultipleCallbacksForSameEvent() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The entity listener [" + ListenerWithMultipleCallbacksForSameEvent.class.getName() +
                "] has multiple callbacks for the lifecycle event [" + LifecycleEvent.POST_LOAD + "].");
        final AbstractIdentifiableType<EntityWithListenerWithMultipleConflictingCallbacks> et = typeFor(
                EntityWithListenerWithMultipleConflictingCallbacks.class);
        resolver.resolve(et);
    }

    private static class ListenerWithMultipleCallbacksForSameEvent {
        public ListenerWithMultipleCallbacksForSameEvent() {
        }

        @PostLoad
        public void firstCallback(Object arg) {
        }

        @PostLoad
        public void secondCallback(Object arg) {
        }
    }

    @EntityListeners(ListenerWithMultipleCallbacksForSameEvent.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithMultipleConflictingCallbacks")
    private static class EntityWithListenerWithMultipleConflictingCallbacks {
    }

    @Test
    public void resolveThrowsMetamodelInitializationExceptionWhenEntityListenerCallbackHasInvalidParameterType() {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("The callback method [callback] in entity listener [" +
                ListenerWithInvalidParameterTypeCallback.class.getName() +
                "] has incorrect signature. Its parameter should be of type [" + Object.class.getName() + "] or [" +
                EntityWithListenerWithInvalidParameterTypeCallback.class.getName() + "].");
        final AbstractIdentifiableType<EntityWithListenerWithInvalidParameterTypeCallback> et = typeFor(
                EntityWithListenerWithInvalidParameterTypeCallback.class);
        resolver.resolve(et);
    }

    private static class ListenerWithInvalidParameterTypeCallback {
        public ListenerWithInvalidParameterTypeCallback() {
        }

        @PostLoad
        public void callback(OWLClassA arg) {
        }
    }

    @EntityListeners(ListenerWithInvalidParameterTypeCallback.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithListenerWithInvalidParameterTypeCallback")
    private static class EntityWithListenerWithInvalidParameterTypeCallback {
    }
}