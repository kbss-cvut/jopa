package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;

class PersistCascadeResolver extends CascadeResolver {

    PersistCascadeResolver(ObjectOntologyMapperImpl mapper) {
        super(mapper);
    }

    @Override
    protected void resolveFieldCascading(FieldSpecification<?, ?> fieldSpec, Object fieldValue, URI context) {
        OWLObjectProperty ann = fieldSpec.getJavaField().getAnnotation(OWLObjectProperty.class);
        assert ann != null;

        for (CascadeType c : ann.cascade()) {
            if (c == CascadeType.ALL || c == CascadeType.PERSIST) {
                return;
            }
        }
        final EntityType<?> et = mapper.getEntityType(fieldValue.getClass());
        final Field idField = et.getIdentifier().getJavaField();
        if (!idField.isAccessible()) {
            idField.setAccessible(true);
        }
        try {
            final URI id = EntityPropertiesUtils.getValueAsURI(idField.get(fieldValue));
            mapper.registerPendingPersist(id, fieldValue, context);
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw new EntityDeconstructionException("Unable to check field cascading.", e);
        }
    }
}
