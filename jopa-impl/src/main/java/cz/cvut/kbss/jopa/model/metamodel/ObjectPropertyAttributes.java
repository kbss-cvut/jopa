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
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;

import java.lang.reflect.Field;

class ObjectPropertyAttributes extends PropertyAttributes {
    public OWLObjectProperty oop ;
    ObjectPropertyAttributes(FieldMappingValidator validator,OWLObjectProperty owlPropertyAnnotation) {
        super(validator);
        assert owlPropertyAnnotation!= null;
        oop = owlPropertyAnnotation;
    }

    @Override
    void resolve( ParticipationConstraints cons, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
        super.resolve(cons, metamodelBuilder, fieldValueCls);

        assert oop != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.OBJECT;
        this.iri = IRI.create(typeBuilderContext.resolveNamespace(oop.iri()));

        if (validator.isValidIdentifierType(fieldValueCls)) {
            initPlainIdentifierAttribute(fieldValueCls);
        } else {
            this.type = metamodelBuilder.getEntityClass(fieldValueCls);
            this.cascadeTypes = oop.cascade();
            this.fetchType = oop.fetch();
        }
    }

    private void initPlainIdentifierAttribute(Class<?> targetType) {
        this.type = BasicTypeImpl.get(targetType);
        this.cascadeTypes = new CascadeType[0];
        this.fetchType = FetchType.EAGER;
    }
}
