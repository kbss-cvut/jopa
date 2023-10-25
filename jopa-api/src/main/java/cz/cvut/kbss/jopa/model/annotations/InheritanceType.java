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

/**
 * Defines polymorphic loading strategy options for the {@link Inheritance} annotation.
 * <p>
 * The strategies are described in <a href="https://github.com/kbss-cvut/jopa/issues/1">https://github.com/kbss-cvut/jopa/issues/1</a>.
 */
public enum InheritanceType {

    /**
     * Instances are loading in two steps. First, types are loaded to determine which resulting type should be used,
     * then the actual entity is loaded.
     */
    TWO_STEP,
    /**
     * An attempt to load an entity is made. If the entity is abstract or the corresponding type is missing, actual type
     * is determined from the loaded types and the missing attributes are loaded.
     */
    TRY_FIRST
}
