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
package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.model.annotations.PostPersist;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;

import java.lang.reflect.Method;

public class ParentListener {

    @PrePersist
    public void prePersist(Object instance) {
        System.out.println("PrePersist");
    }

    @PostPersist
    private void postPersist(Object instance) {
        System.out.println("Private postPersist");
    }

    public static Method getPrePersistMethod() throws Exception {
        return ParentListener.class.getDeclaredMethod("prePersist", Object.class);
    }

    public static Method getPostPersistMethod() throws Exception {
        return ParentListener.class.getDeclaredMethod("postPersist", Object.class);
    }
}
