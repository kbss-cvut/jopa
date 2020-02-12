/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

/**
 * Instances of the type MapAttribute represent persistent java.util.Map-valued
 * attributes.
 *
 * @param <X>
 *            The type the represented Map belongs to
 * @param <K>
 *            The type of the key of the represented Map
 * @param <V>
 *            The type of the value of the represented Map
 */
public interface MapAttribute<X, K, V> extends PluralAttribute<X, java.util.Map<K, V>, V> {
    /**
     * Return the Java type of the map key.
     *
     * @return Java key type
     */
    Class<K> getKeyJavaType();

    /**
     * Return the type representing the key type of the map.
     *
     * @return type representing key type
     */
    Type<K> getKeyType();
}
