/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import static org.mockito.Mockito.*;

class EntityLifecycleListenerManagerTest {

    private EntityLifecycleListenerManager manager = new EntityLifecycleListenerManager();

    @Test
    void listenerInvocationInvokesCorrectCallback() throws Exception {
        manager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Child.class.getDeclaredMethod("prePersistChild"));
        final Child instance = spy(new Child());
        manager.invokePrePersistCallbacks(instance);
        verify(instance).prePersistChild();
    }

    @Test
    void listenerInvocationDoesNothingWhenNoMatchingListenerExists() {
        // The callback is not registered
        final Child instance = spy(new Child());
        manager.invokePrePersistCallbacks(instance);
        verify(instance, never()).prePersistChild();
    }

    @Test
    void listenerInvocationInvokesListenersTopDown() throws Exception {
        final EntityLifecycleListenerManager parentManager = new EntityLifecycleListenerManager();
        parentManager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Parent.class.getDeclaredMethod("prePersist"));
        manager.setParent(parentManager);
        manager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Child.class.getDeclaredMethod("prePersistChild"));
        final Child instance = spy(new Child());
        manager.invokePrePersistCallbacks(instance);
        final InOrder inOrder = inOrder(instance);
        inOrder.verify(instance).prePersist();
        inOrder.verify(instance).prePersistChild();
    }

    @Test
    void listenerInvocationInvokesAncestorListenersWhenNoneAreDeclaredDirectlyOnEntity() throws Exception {
        final EntityLifecycleListenerManager parentManager = new EntityLifecycleListenerManager();
        parentManager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Parent.class.getDeclaredMethod("prePersist"));
        manager.setParent(parentManager);
        final Child instance = spy(new Child());
        manager.invokePrePersistCallbacks(instance);
        verify(instance).prePersist();
        verify(instance, never()).prePersistChild();
    }

    @SuppressWarnings("unused")
    private static class ParentListener {
        @PostLoad
        void postLoad(Parent instance) {
        }
    }

    @SuppressWarnings("unused")
    private static class ChildListener {
        @PostLoad
        void postLoad(Object instance) {
        }
    }

    @SuppressWarnings("unused")
    private static class AnotherChildListener {
        @PostLoad
        void postLoad(Child instance) {
        }
    }

    @EntityListeners(ParentListener.class)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "Parent")
    private static class Parent {

        @PrePersist
        void prePersist() {
        }

        @PostLoad
        void postLoad() {
        }
    }

    @EntityListeners({ChildListener.class, AnotherChildListener.class})
    @OWLClass(iri = Vocabulary.CLASS_BASE + "Child")
    private static class Child extends Parent {

        @PrePersist
        void prePersistChild() {
        }

        @PostLoad
        void postLoadChild() {
        }
    }

    @Test
    void listenerInvocationInvokesEntityListenerCallbacks() throws Exception {
        final ParentListener listener = spy(new ParentListener());
        manager.addEntityListener(listener);
        manager.addEntityListenerCallback(listener, LifecycleEvent.POST_LOAD,
                ParentListener.class.getDeclaredMethod("postLoad", Parent.class));
        final Parent instance = new Parent();
        manager.invokePostLoadCallbacks(instance);
        verify(listener).postLoad(instance);
    }

    @Test
    void listenerInvocationInvokesEntityListenerCallbacksTopDown() throws Exception {
        final EntityLifecycleListenerManager parentManager = new EntityLifecycleListenerManager();
        final ParentListener parentListener = spy(new ParentListener());
        parentManager.addEntityListener(parentListener);
        parentManager.addEntityListenerCallback(parentListener, LifecycleEvent.POST_LOAD,
                ParentListener.class.getDeclaredMethod("postLoad", Parent.class));
        manager.setParent(parentManager);
        final ChildListener childListener = spy(new ChildListener());
        manager.addEntityListener(childListener);
        manager.addEntityListenerCallback(childListener, LifecycleEvent.POST_LOAD,
                ChildListener.class.getDeclaredMethod("postLoad", Object.class));
        final Child instance = new Child();
        manager.invokePostLoadCallbacks(instance);
        final InOrder inOrder = inOrder(parentListener, childListener);
        inOrder.verify(parentListener).postLoad(instance);
        inOrder.verify(childListener).postLoad(instance);
    }

    @Test
    void listenerInvocationInvokesEntityListenersInOrderOfDeclarationOnEntity() throws Exception {
        final ChildListener childListener = spy(new ChildListener());
        manager.addEntityListener(childListener);
        manager.addEntityListenerCallback(childListener, LifecycleEvent.POST_LOAD,
                ChildListener.class.getDeclaredMethod("postLoad", Object.class));
        final AnotherChildListener anotherChildListener = spy(new AnotherChildListener());
        manager.addEntityListener(anotherChildListener);
        manager.addEntityListenerCallback(anotherChildListener, LifecycleEvent.POST_LOAD,
                AnotherChildListener.class.getDeclaredMethod("postLoad", Child.class));
        final Child instance = new Child();
        manager.invokePostLoadCallbacks(instance);

        final InOrder inOrder = inOrder(childListener, anotherChildListener);
        inOrder.verify(childListener).postLoad(instance);
        inOrder.verify(anotherChildListener).postLoad(instance);
    }

    @Test
    void listenerInvocationInvokesEntityListenerCallbacksBeforeInternalLifecycleCallbacks() throws Exception {
        final EntityLifecycleListenerManager parentManager = new EntityLifecycleListenerManager();
        final ParentListener parentListener = spy(new ParentListener());
        parentManager.addEntityListener(parentListener);
        parentManager.addEntityListenerCallback(parentListener, LifecycleEvent.POST_LOAD,
                ParentListener.class.getDeclaredMethod("postLoad", Parent.class));
        parentManager.addLifecycleCallback(LifecycleEvent.POST_LOAD, Parent.class.getDeclaredMethod("postLoad"));
        manager.setParent(parentManager);
        manager.setParent(parentManager);
        final ChildListener childListener = spy(new ChildListener());
        manager.addEntityListener(childListener);
        manager.addEntityListenerCallback(childListener, LifecycleEvent.POST_LOAD,
                ChildListener.class.getDeclaredMethod("postLoad", Object.class));
        manager.addLifecycleCallback(LifecycleEvent.POST_LOAD, Child.class.getDeclaredMethod("postLoadChild"));
        final Child instance = spy(new Child());
        manager.invokePostLoadCallbacks(instance);

        final InOrder inOrder = inOrder(parentListener, childListener, instance);
        inOrder.verify(parentListener).postLoad(instance);
        inOrder.verify(childListener).postLoad(instance);
        inOrder.verify(instance).postLoad();
        inOrder.verify(instance).postLoadChild();
    }

    @Test
    void listenerInvocationPreventsEndlessLoopWhenListenerImplementationWouldCauseAnotherListenerToFire()
            throws Exception {
        manager.addLifecycleCallback(LifecycleEvent.PRE_UPDATE,
                EntityWithLoopingListener.class.getDeclaredMethod("preUpdate"));
        final EntityWithLoopingListener instance = spy(new EntityWithLoopingListener());
        manager.invokePreUpdateCallbacks(instance);
        verify(instance).preUpdate();   // Exactly once
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "EntityWithLoopingListener")
    private class EntityWithLoopingListener {

        @PreUpdate
        void preUpdate() {
            // This simulates the listener being called from UOW after an entity lifecycle event (e.g. setting an attribute)
            EntityLifecycleListenerManagerTest.this.manager.invokePreUpdateCallbacks(this);
        }
    }
}
