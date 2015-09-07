package cz.cvut.kbss.jopa.owlapi.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.owlapi.BasicTypeImpl;
import cz.cvut.kbss.jopa.owlapi.MetamodelImpl;

import java.lang.reflect.Field;

/**
 * @author ledvima1
 */
class AnnotationPropertyAttributes extends PropertyAttributes {

    @Override
    void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls) {
        final OWLAnnotationProperty oap = field.getAnnotation(OWLAnnotationProperty.class);
        assert oap != null;
        persistentAttributeType = Attribute.PersistentAttributeType.ANNOTATION;
        iri = IRI.create(oap.iri());
        type = BasicTypeImpl.get(fieldValueCls);
    }
}
