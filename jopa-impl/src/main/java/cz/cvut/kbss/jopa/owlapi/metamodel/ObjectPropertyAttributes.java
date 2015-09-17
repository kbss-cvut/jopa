package cz.cvut.kbss.jopa.owlapi.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;

import java.lang.reflect.Field;

class ObjectPropertyAttributes extends PropertyAttributes {

    @Override
    void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls) {
        super.resolve(field, metamodel, fieldValueCls);
        final OWLObjectProperty oop = field.getAnnotation(OWLObjectProperty.class);
        assert oop != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.OBJECT;
        this.iri = IRI.create(oop.iri());
        this.cascadeTypes = oop.cascade();
        this.fetchType = oop.fetch();
        this.type = metamodel.entity(fieldValueCls);
    }
}
