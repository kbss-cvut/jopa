/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.EnumType;
import cz.cvut.kbss.jopa.model.annotations.Enumerated;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;

abstract class PropertyAttributes {

    final FieldMappingValidator validator;

    TypeBuilderContext<?> typeBuilderContext;

    Type<?> type = null;
    Attribute.PersistentAttributeType persistentAttributeType = null;
    IRI iri = null;
    CascadeType[] cascadeTypes = new CascadeType[]{};
    FetchType fetchType = FetchType.EAGER;
    boolean lexicalForm = false;
    boolean simpleLiteral = false;
    String datatype = "";
    String language;
    private boolean nonEmpty = false;
    private ParticipationConstraint[] participationConstraints = new ParticipationConstraint[]{};
    private EnumType enumType = null;

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

    boolean isLexicalForm() {
        return lexicalForm;
    }

    boolean isSimpleLiteral() {
        return simpleLiteral;
    }

    public String getDatatype() {
        return datatype;
    }

    public boolean hasDatatype() {
        return !datatype.isEmpty();
    }

    String getLanguage() {
        return language;
    }

    ParticipationConstraint[] getParticipationConstraints() {
        return participationConstraints;
    }
    public EnumType getEnumType() {
        return enumType;
    }
    void resolve(PropertyInfo propertyInfo, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
        resolveParticipationConstraints(propertyInfo);
        resolveEnumType(propertyInfo, fieldValueCls);
    }

    private void resolveParticipationConstraints(PropertyInfo propertyInfo) {
        ParticipationConstraints cons = propertyInfo.getAnnotation(ParticipationConstraints.class);

        if (cons != null) {
            if (cons.value().length > 0) {
                this.participationConstraints = cons.value();
            } else {
                this.nonEmpty = cons.nonEmpty();
            }
        }
    }
    private void resolveEnumType(PropertyInfo propertyInfo, Class<?> fieldValueCls) {
        final Enumerated enumAnn = propertyInfo.getAnnotation(Enumerated.class);
        if (enumAnn != null) {
            this.enumType = enumAnn.value();
        } else  if (fieldValueCls.isEnum()) {
            // As per default of Enumerated.value()
            this.enumType = EnumType.STRING;
        }
    }

    String resolveLanguage(Class<?> fieldValueCls) {
        return MultilingualString.class.equals(fieldValueCls) ? null : typeBuilderContext.getPuLanguage();
    }

    static PropertyAttributes create(PropertyInfo field, FieldMappingValidator validator, TypeBuilderContext<?> context) {
        final PropertyAttributes instance;
        if (field.getAnnotation(OWLObjectProperty.class) != null) {
            instance = new ObjectPropertyAttributes(validator);
        } else if (field.getAnnotation(OWLDataProperty.class) != null) {
            instance = new DataPropertyAttributes(validator);
        } else if (field.getAnnotation(OWLAnnotationProperty.class) != null) {
            instance = new AnnotationPropertyAttributes(validator);
        } else {
            instance = new NonPropertyAttributes(validator);
        }
        instance.typeBuilderContext = context;
        return instance;
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
        void resolve(PropertyInfo propertyInfo, MetamodelBuilder metamodelBuilder, Class<?> fieldValueCls) {
            // do nothing
        }
    }
}
