/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Allows to specify participation constraints of an attribute.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD,ElementType.METHOD})
public @interface ParticipationConstraints {

    ParticipationConstraint[] value() default {};

    /**
     * (Optional) Whether the annotated field can be empty, i.e. {@code null} for singular attributes and {@code null}
     * or empty for plural attributes (this is up to the implementation).
     * <p>
     * This attributes corresponds to using a {@link ParticipationConstraint} with {@code min = 1}.
     * <p>
     * In case there are participation constraints specified as values of this annotation, the {@code nonEmpty} value is
     * ignored.
     *
     * @return Whether the field can be empty
     */
    boolean nonEmpty() default false;
}
