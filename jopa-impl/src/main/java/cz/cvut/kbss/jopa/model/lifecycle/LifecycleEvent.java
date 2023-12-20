/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
