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

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface ParticipationConstraint {

    /**
     * IRI of the property filler
     *
     * @return IRI of the property filler
     */
    String owlObjectIRI();

    /**
     * Minimal cardinality.
     *
     * @return minimal cardinality
     */
    int min() default 0;

    /**
     * Maximal cardinality.
     * <p>
     * Negative integer means positive infinity
     *
     * @return maximal cardinality
     */
    int max() default -1;

}
