/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the inheritance strategy to be used for an entity class hierarchy.
 * <p>
 * It is specified on the entity class that is the root of the entity class hierarchy. If the {@link Inheritance}
 * annotation is not specified or if no inheritance type is specified for an entity class hierarchy, the TWO_STEP
 * loading strategy is used.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Inheritance {

    /**
     * The strategy to be used for the entity inheritance hierarchy.
     *
     * @return Inheritance strategy
     */
    InheritanceType strategy() default InheritanceType.TWO_STEP;
}
