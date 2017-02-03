package cz.cvut.kbss.jopa.model.lifecycle;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.annotation.Annotation;

/**
 * Entity lifecycle events.
 */
public enum LifecycleEvent {
    PRE_PERSIST(PrePersist.class), POST_PERSIST(PostPersist.class), PRE_REMOVE(PreRemove.class),
    POST_REMOVE(PostRemove.class), PRE_UPDATE(PreUpdate.class), POST_UPDATE(PostUpdate.class),
    POST_LOAD(PostLoad.class);

    private final Class<? extends Annotation> annotation;

    LifecycleEvent(Class<? extends Annotation> annotation) {
        this.annotation = annotation;
    }

    public Class<? extends Annotation> getAnnotation() {
        return annotation;
    }
}
