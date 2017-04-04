package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import org.junit.Test;

import static org.junit.Assert.*;

public class EntityLifecycleListenerManagerTest {

    private EntityLifecycleListenerManager manager = new EntityLifecycleListenerManager();

    @Test
    public void listenerInvocationInvokesCorrectListener() throws Exception {
        manager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Child.class.getDeclaredMethod("prePersistChild"));
        final Child instance = new Child();
        manager.invokePrePersistListeners(instance);
        assertTrue(instance.childCalled);
    }

    @Test
    public void listenerInvocationDoesNothingWhenNoMatchingListenerExists() throws Exception {
        // The callback is not registered
        final Child instance = new Child();
        manager.invokePrePersistListeners(instance);
        assertFalse(instance.childCalled);
    }

    @Test
    public void listenerInvocationInvokesAlsoAncestorListeners() throws Exception {
        final EntityLifecycleListenerManager parentManager = new EntityLifecycleListenerManager();
        parentManager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Parent.class.getDeclaredMethod("prePersist"));
        manager.setParent(parentManager);
        manager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Child.class.getDeclaredMethod("prePersistChild"));
        final Child instance = new Child();
        manager.invokePrePersistListeners(instance);
        assertTrue(instance.childCalled);
        assertTrue(instance.parentCalled);
    }

    @Test
    public void listenerInvocationInvokesAncestorListenersWhenNoneAreDeclaredDirectlyOnEntity() throws Exception {
        final EntityLifecycleListenerManager parentManager = new EntityLifecycleListenerManager();
        parentManager.addLifecycleCallback(LifecycleEvent.PRE_PERSIST, Parent.class.getDeclaredMethod("prePersist"));
        manager.setParent(parentManager);
        final Child instance = new Child();
        manager.invokePrePersistListeners(instance);
        assertTrue(instance.parentCalled);
        assertFalse(instance.childCalled);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "Parent")
    private static class Parent {

        boolean parentCalled = false;

        @PrePersist
        private void prePersist() {
            this.parentCalled = true;
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "Child")
    private static class Child extends Parent {
        private boolean childCalled = false;

        @PrePersist
        private void prePersistChild() {
            this.childCalled = true;
        }
    }
}