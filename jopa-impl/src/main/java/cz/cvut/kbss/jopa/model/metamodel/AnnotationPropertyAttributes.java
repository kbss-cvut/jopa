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
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;


class AnnotationPropertyAttributes extends PropertyAttributes {

    AnnotationPropertyAttributes(FieldMappingValidator validator) {
        super(validator);
    }

    @Override
    void resolve(PropertyInfo propertyInfo, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
        super.resolve(propertyInfo, metamodelBuilder, fieldValueCls);
        final OWLAnnotationProperty oap = propertyInfo.getAnnotation(OWLAnnotationProperty.class);
        assert oap != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.ANNOTATION;
        this.iri = IRI.create(typeBuilderContext.resolveNamespace(oap.iri()));
        this.fetchType = oap.fetch();
        this.type = BasicTypeImpl.get(fieldValueCls);
        this.lexicalForm = oap.lexicalForm();
        this.simpleLiteral = oap.simpleLiteral();
        this.datatype = typeBuilderContext.resolveNamespace(oap.datatype());
        this.language = resolveLanguage(fieldValueCls);
    }
}
