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
package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Method;

@SuppressWarnings("unused")
public class ConcreteListener {

    @PrePersist
    public void prePersist(OWLClassR instance) {
    }

    @PostPersist
    public void postPersist(Object instance) {
    }

    @PreUpdate
    public void preUpdate(Object instance) {
    }

    @PostUpdate
    public void postUpdate(OWLClassR instance) {
    }

    @PreRemove
    public void preRemove(OWLClassR instance) {
    }

    @PostRemove
    public void postRemove(Object instance) {
    }

    @PostLoad
    public void postLoad(Object instance) {
    }

    public static Method getPrePersist() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("prePersist", OWLClassR.class);
    }

    public static Method getPostPersist() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("postPersist", Object.class);
    }

    public static Method getPreUpdate() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("preUpdate", Object.class);
    }

    public static Method getPostUpdate() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("postUpdate", OWLClassR.class);
    }

    public static Method getPreRemove() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("preRemove", OWLClassR.class);
    }

    public static Method getPostRemove() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("postRemove", Object.class);
    }

    public static Method getPostLoad() throws NoSuchMethodException {
        return ConcreteListener.class.getDeclaredMethod("postLoad", Object.class);
    }
}
