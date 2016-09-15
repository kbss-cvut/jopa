/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;

public class EntityTypeImpl<X> extends AbstractIdentifiableType<X> implements EntityType<X> {

    private final String name;

    private final IRI iri;

    public EntityTypeImpl(String name, Class<X> javaType, final IRI iri) {
        super(javaType);
        this.name = name;
        this.iri = iri;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public PersistenceType getPersistenceType() {
        return PersistenceType.ENTITY;
    }

    @Override
    public Class<X> getBindableJavaType() {
        return getJavaType();
    }

    @Override
    public BindableType getBindableType() {
        return BindableType.ENTITY_TYPE;
    }

    @Override
    public IRI getIRI() {
        return iri;
    }

    @Override
    public String toString() {
        return "EntityType{" + name + "<" + iri + ">}";
    }
}
