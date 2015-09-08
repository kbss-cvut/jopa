package cz.cvut.kbss.jopa.owlapi.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;

import java.lang.reflect.Field;

/**
 * @author ledvima1
 */
class ObjectPropertyAttributes extends PropertyAttributes {

    @Override
    void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls) {
        super.resolve(field, metamodel, fieldValueCls);
        final OWLObjectProperty oop = field.getAnnotation(OWLObjectProperty.class);
        assert oop != null;

        persistentAttributeType = Attribute.PersistentAttributeType.OBJECT;
        iri = IRI.create(oop.iri());
        cascadeTypes = oop.cascade();
        fetchType = oop.fetch();
        type = metamodel.entity(fieldValueCls);
    }
}
