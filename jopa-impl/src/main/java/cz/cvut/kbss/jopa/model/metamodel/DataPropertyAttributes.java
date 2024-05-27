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
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

class DataPropertyAttributes extends PropertyAttributes {

    DataPropertyAttributes(FieldMappingValidator validator) {
        super(validator);
    }

    @Override
    void resolve(PropertyInfo propertyInfo, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
        super.resolve(propertyInfo, metamodelBuilder, fieldValueCls);

        final OWLDataProperty odp = propertyInfo.getAnnotation(OWLDataProperty.class);
        assert odp != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.DATA;
        this.iri = IRI.create(typeBuilderContext.resolveNamespace(odp.iri()));
        this.fetchType = FetchType.EAGER;
        this.type = BasicTypeImpl.get(fieldValueCls);
        this.lexicalForm = odp.lexicalForm();
        this.simpleLiteral = odp.simpleLiteral();
        this.datatype = typeBuilderContext.resolveNamespace(odp.datatype());
        this.language = resolveLanguage(fieldValueCls);
    }
}
