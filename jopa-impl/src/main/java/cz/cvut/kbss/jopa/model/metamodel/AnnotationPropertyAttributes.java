/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;

import java.lang.reflect.Field;

class AnnotationPropertyAttributes extends PropertyAttributes {

    AnnotationPropertyAttributes(FieldMappingValidator validator) {
        super(validator);
    }

    @Override
    void resolve(Field field, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
        super.resolve(field, metamodelBuilder, fieldValueCls);
        final OWLAnnotationProperty oap = field.getAnnotation(OWLAnnotationProperty.class);
        assert oap != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.ANNOTATION;
        this.iri = IRI.create(typeBuilderContext.resolveNamespace(oap.iri()));
        this.fetchType = oap.fetch();
        this.type = BasicTypeImpl.get(fieldValueCls);
        this.lexicalForm = oap.lexicalForm();
        this.simpleLiteral = oap.simpleLiteral();
        this.language = resolveLanguage(fieldValueCls);
        validator.validateAnnotationPropertyField(field, oap);
    }
}
