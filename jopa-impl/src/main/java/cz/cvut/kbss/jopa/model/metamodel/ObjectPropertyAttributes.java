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
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;

class ObjectPropertyAttributes extends PropertyAttributes {
    ObjectPropertyAttributes(FieldMappingValidator validator) {
        super(validator);
    }

    @Override
    void resolve(PropertyInfo propertyInfo, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
        super.resolve(propertyInfo, metamodelBuilder, fieldValueCls);

        final OWLObjectProperty oop = propertyInfo.getAnnotation(OWLObjectProperty.class);
        assert oop != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.OBJECT;
        this.iri = IRI.create(typeBuilderContext.resolveNamespace(oop.iri()));

        if (validator.isValidIdentifierType(fieldValueCls) || fieldValueCls.isEnum()) {
            initBasicTypeAttribute(fieldValueCls);
        } else {
            this.type = metamodelBuilder.getEntityClass(fieldValueCls);
            this.cascadeTypes = oop.cascade();
            this.fetchType = oop.fetch();
        }
    }

    private void initBasicTypeAttribute(Class<?> targetType) {
        this.type = BasicTypeImpl.get(targetType);
        this.cascadeTypes = new CascadeType[0];
        this.fetchType = FetchType.EAGER;
    }
}
