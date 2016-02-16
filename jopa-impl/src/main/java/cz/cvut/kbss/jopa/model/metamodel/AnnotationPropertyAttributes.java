package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.BasicTypeImpl;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;

import java.lang.reflect.Field;

/**
 * @author ledvima1
 */
class AnnotationPropertyAttributes extends PropertyAttributes {

    @Override
    void resolve(Field field, MetamodelImpl metamodel, Class<?> fieldValueCls) {
        super.resolve(field, metamodel, fieldValueCls);
        final OWLAnnotationProperty oap = field.getAnnotation(OWLAnnotationProperty.class);
        assert oap != null;

        this.persistentAttributeType = Attribute.PersistentAttributeType.ANNOTATION;
        this.iri = IRI.create(oap.iri());
        this.fetchType = oap.fetch();
        this.type = BasicTypeImpl.get(fieldValueCls);
    }
}
