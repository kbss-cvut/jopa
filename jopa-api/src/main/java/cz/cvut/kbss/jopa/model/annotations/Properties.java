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

import java.lang.annotation.*;

/**
 * Denotes an attribute containing values of unmapped properties relevant for an entity.
 * <p>
 * The attribute has to be a {@link java.util.Map} of keys which are of a valid identifier type and represent
 * ontological properties, e.g. {@link String}, and {@link java.util.Set}s of values of the properties. The type of the
 * values can be either {@link String} or it can be {@link Object}, in which case the respective ontological type will
 * be retained.
 * <p>
 * For example:
 *
 * <pre><code>Map&lt;String, Set&lt;String&gt;&gt;</code></pre>
 * <pre><code>Map&lt;URI, Set&lt;Object&gt;&gt;</code></pre>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Properties {

    FetchType fetchType() default FetchType.LAZY;

    /**
     * Denotes a member that is inferred (true) using the OWL reasoner or just asserted (false).
     *
     * @return Whether this property is read only
     */
    boolean readOnly() default false;
}
