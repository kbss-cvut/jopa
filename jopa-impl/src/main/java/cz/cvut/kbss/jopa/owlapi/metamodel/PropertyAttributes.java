package cz.cvut.kbss.jopa.owlapi.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;

import java.lang.reflect.Field;

/**
 * @author ledvima1
 */
abstract class PropertyAttributes {

    Type<?> type = null;
    Attribute.PersistentAttributeType persistentAttributeType = null;
    IRI iri = null;
    CascadeType[] cascadeTypes = new CascadeType[]{};
    FetchType fetchType = FetchType.EAGER;
    boolean nonEmpty = false;
    ParticipationConstraint[] participationConstraints = new ParticipationConstraint[]{};

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

    static PropertyAttributes create(Field field) {
        if (field.getAnnotation(OWLObjectProperty.class) != null) {
            return new ObjectPropertyAttributes();
        } else if (field.getAnnotation(OWLDataProperty.class) != null) {
            return new DataPropertyAttributes();
        } else if (field.getAnnotation(OWLAnnotationProperty.class) != null) {
            return new AnnotationPropertyAttributes();
        } else {
            return new NonPropertyAttributes();
        }
    }

    private static class NonPropertyAttributes extends PropertyAttributes {

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
