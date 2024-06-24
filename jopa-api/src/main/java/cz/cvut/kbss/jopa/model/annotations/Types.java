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
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Set;

/**
 * Represents the set of ontological types to which an entity belongs.
 * <p>
 * Note that this set does NOT contain the primary type mapped by the entity class itself.
 * <p>
 * The annotated field has to be a {@link Set} of valid identifier types, e.g. {@link String}, {@link java.net.URI}.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Types {

    FetchType fetchType() default FetchType.EAGER;

    /**
     * Denotes a member that is inferred (true) using the OWL reasoner or just asserted (false).
     *
     * @return Whether this property is read only
     */
    boolean readOnly() default false;
}
