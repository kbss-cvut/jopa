/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.InheritanceType;

/**
 *  Instances of the type IdentifiableEntityType represent entity
 *  types which can be saved to and read from storage.
 *
 * @param <X> Entity type being represented by this instance
 */
public abstract class IdentifiableEntityType<X> extends AbstractIdentifiableType<X> implements EntityType<X> {

    private final String name;

    private final IRI iri;

    private InheritanceType inheritanceType;

    public IdentifiableEntityType(Class<X> javaType, final IRI iri) {
        super(javaType);
        this.name = javaType.getSimpleName();
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


    /**
     * Gets inheritance type of this entity type.
     * <p>
     * If the entity type is a root if an inheritance hierarchy, the type can be defined using the {@link
     * cz.cvut.kbss.jopa.model.annotations.Inheritance} annotation. If the entity is deeper in inheritance hierarchy, it
     * is inherited from the supertype. Otherwise, it defaults to {@link cz.cvut.kbss.jopa.utils.Constants#DEFAULT_INHERITANCE_TYPE}.
     *
     * @return Inheritance strategy for this entity type
     */
    public InheritanceType getInheritanceType() {
        return inheritanceType;
    }

    void setInheritanceType(InheritanceType inheritanceType) {
        this.inheritanceType = inheritanceType;
    }

    @Override
    public String toString() {
        return "EntityType{" + name + "<" + iri + ">}";
    }
}
