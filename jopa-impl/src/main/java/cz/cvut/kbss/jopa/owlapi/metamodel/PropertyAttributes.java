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

    public Type<?> getType() {
        return type;
    }

    public Attribute.PersistentAttributeType getPersistentAttributeType() {
        return persistentAttributeType;
    }

    public IRI getIri() {
        return iri;
    }

    public CascadeType[] getCascadeTypes() {
        return cascadeTypes;
    }

    public FetchType getFetchType() {
        return fetchType;
    }

    boolean isKnownOwlProperty() {
        return true;
    }

    abstract void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls);

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
