/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies that the annotated class is a converter and defines its scope.
 * <p>
 * A converter class must be annotated with the Converter annotation. If the autoApply element is specified as true, the
 * persistence provider must automatically apply the converter to all mapped attributes of the specified target type for
 * all entities in the persistence unit.
 * <p>
 * Note that Id attributes, version attributes, relationship attributes, and attributes explicitly annotated as
 * Enumerated or Temporal will not be converted.
 * <p>
 * Note that if autoApply is true, the Convert annotation may be used to override or disable auto-apply conversion on a
 * per-attribute basis.
 * <p>
 * If autoApply is false, only those attributes of the target type for which the Convert annotation has been specified
 * will be converted.
 * <p>
 * If there is more than one converter defined for the same target type, the Convert annotation should be used to
 * explicitly specify which converter to use.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Converter {

    /**
     * Whether to apply the converter automatically.
     */
    boolean autoApply() default false;
}
