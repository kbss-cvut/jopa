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

import java.util.Set;

public class EntityTypeImpl<X> extends AbstractIdentifiableType<X> implements EntityType<X> {

    private final String name;

    private final IRI iri;

    private Identifier identifier;

    private TypesSpecification<X, ?> directTypes;

    private PropertiesSpecification<X, ?, ?, ?> properties;

    public EntityTypeImpl(String name, Class<X> javaType, final IRI iri) {
        super(javaType);
        this.name = name;
        this.iri = iri;
    }

    public void addDirectTypes(TypesSpecification<X, ?> a) {
        this.directTypes = a;
    }

    public void addOtherProperties(PropertiesSpecification<X, ?, ?, ?> a) {
        this.properties = a;
    }

    public void setIdentifier(final Identifier identifier) {
        this.identifier = identifier;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Type.PersistenceType getPersistenceType() {
        return PersistenceType.ENTITY;
    }

    @Override
    public Class<X> getBindableJavaType() {
        return getJavaType();
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
        return BindableType.ENTITY_TYPE;
    }

    @Override
    public IRI getIRI() {
        return iri;
    }

    @Override
    public Identifier getIdentifier() {
        return identifier;
    }

    @Override
    public TypesSpecification<? super X, ?> getTypes() {
        return directTypes;
    }

    @Override
    public PropertiesSpecification<? super X, ?, ?, ?> getProperties() {
        return properties;
    }

    @Override
    public FieldSpecification<? super X, ?> getFieldSpecification(String fieldName) {
        // TODO
//        FieldSpecification<? super X, ?> att = attributeMap.get(fieldName);
//        if (att == null) {
//            if (directTypes != null && directTypes.getName().equals(fieldName)) {
//                att = directTypes;
//            } else if (properties != null && properties.getName().equals(fieldName)) {
//                att = properties;
//            }
//        }
//        if (att == null) {
//            throw new IllegalArgumentException("Field " + fieldName + " is not declared in type " + name);
//        }
//        return att;
        return null;
    }

    @Override
    public Set<FieldSpecification<? super X, ?>> getFieldSpecifications() {
        // TODO
//        final Set<FieldSpecification<? super X, ?>> specs = new HashSet<>(attributeMap.values());
//        if (directTypes != null) {
//            specs.add(directTypes);
//        }
//        if (properties != null) {
//            specs.add(properties);
//        }
//        return specs;
        return null;
    }

    @Override
    public String toString() {
        return "EntityType{" + name + "<" + iri + ">}";
    }
}
