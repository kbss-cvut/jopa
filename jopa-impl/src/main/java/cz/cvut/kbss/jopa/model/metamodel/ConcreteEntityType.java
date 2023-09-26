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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;

/**
 * Instances of the type ConcreteEntityType represent entity types that can be directly instantiated after being loaded from storage.
 *
 * @param <X> Entity type being represented by this instance
 */
public class ConcreteEntityType<X> extends IdentifiableEntityType<X> {

    /**
     * The java type that can be used to instantiate an object of the entity class represented by this entity type.
     */
    private final Class<? extends X> instantiableType;

    public ConcreteEntityType(String name, Class<X> javaType, IRI iri) {
        super(name, javaType, iri);
        // TODO for now
        this.instantiableType = javaType;
    }

    @Override
    public boolean isAbstract() {
        return false;
    }

    @Override
    public Class<? extends X> getInstantiableJavaType() {
        return instantiableType;
    }
}
