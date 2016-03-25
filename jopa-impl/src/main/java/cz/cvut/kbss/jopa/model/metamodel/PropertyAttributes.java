/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.*;

import java.lang.reflect.Field;

abstract class PropertyAttributes {

    final FieldMappingValidator validator;

    Type<?> type = null;
    Attribute.PersistentAttributeType persistentAttributeType = null;
    IRI iri = null;
    CascadeType[] cascadeTypes = new CascadeType[]{};
    FetchType fetchType = FetchType.EAGER;
    boolean nonEmpty = false;
    ParticipationConstraint[] participationConstraints = new ParticipationConstraint[]{};

    PropertyAttributes(FieldMappingValidator validator) {
        this.validator = validator;
    }

    Type<?> getType() {
        return type;
    }

    Attribute.PersistentAttributeType getPersistentAttributeType() {
        return persistentAttributeType;
    }

    IRI getIri() {
        return iri;
    }

    CascadeType[] getCascadeTypes() {
        return cascadeTypes;
    }

    FetchType getFetchType() {
        return fetchType;
    }

    boolean isKnownOwlProperty() {
        return true;
    }

    boolean isNonEmpty() {
        return nonEmpty;
    }

    ParticipationConstraint[] getParticipationConstraints() {
        return participationConstraints;
    }

    void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls) {
        resolveParticipationConstraints(field);
    }

    void resolveParticipationConstraints(Field field) {
        ParticipationConstraints cons = field.getAnnotation(ParticipationConstraints.class);
        if (cons != null) {
            if (cons.value() != null && cons.value().length > 0) {
                this.participationConstraints = cons.value();
            } else {
                this.nonEmpty = cons.nonEmpty();
            }
        }
    }

    static PropertyAttributes create(Field field, FieldMappingValidator validator) {
        if (field.getAnnotation(OWLObjectProperty.class) != null) {
            return new ObjectPropertyAttributes(validator);
        } else if (field.getAnnotation(OWLDataProperty.class) != null) {
            return new DataPropertyAttributes(validator);
        } else if (field.getAnnotation(OWLAnnotationProperty.class) != null) {
            return new AnnotationPropertyAttributes(validator);
        } else {
            return new NonPropertyAttributes(validator);
        }
    }

    private static class NonPropertyAttributes extends PropertyAttributes {

        NonPropertyAttributes(FieldMappingValidator validator) {
            super(validator);
        }

        @Override
        boolean isKnownOwlProperty() {
            return false;
        }

        @Override
        void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls) {
            // do nothing
        }
    }
}
