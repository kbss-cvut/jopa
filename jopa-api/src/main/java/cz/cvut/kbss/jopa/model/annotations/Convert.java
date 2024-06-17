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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the conversion of a Basic field or property.
 * <p>
 * The {@code Convert} annotation should not be used to specify conversion of the following: Id attributes, version
 * attributes, relationship attributes, and attributes explicitly denoted as Enumerated or Temporal. Applications that
 * specify such conversions will not be portable.
 * <p>
 * The {@code Convert} annotation may be applied to a basic attribute or to an element collection of basic type (in
 * which case the converter is applied to the elements of the collection).
 */
@Target({ElementType.FIELD,ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Convert {

    /**
     * Specifies the converter to be applied.
     * <p>
     * A value for this element must be specified if multiple converters would otherwise apply.
     */
    Class<?> converter() default void.class;

    /**
     * Used to disable an auto-apply converter. If disableConversion is true, the converter element should not be
     * specified.
     */
    boolean disableConversion() default false;
}
