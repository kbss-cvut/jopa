/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.annotations.InheritanceType;

public abstract class EntityTypeImpl<X> extends AbstractIdentifiableType<X> implements EntityType<X> {

    private final String name;

    private final IRI iri;

    private InheritanceType inheritanceType;

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
    void setSupertype(AbstractIdentifiableType<? super X> supertype) {
        super.setSupertype(supertype);
        if (supertype.getPersistenceType() == PersistenceType.ENTITY) {
            this.inheritanceType = ((EntityTypeImpl) supertype).inheritanceType;
        }
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
