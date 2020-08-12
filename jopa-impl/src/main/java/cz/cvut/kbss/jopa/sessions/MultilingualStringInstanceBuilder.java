package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;

class MultilingualStringInstanceBuilder extends AbstractInstanceBuilder {

    MultilingualStringInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration cloneConfiguration) {
        if (original == null) {
            return null;
        }
        assert original instanceof MultilingualString;
        return new MultilingualString(((MultilingualString) original).getValue());
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        EntityPropertiesUtils.setFieldValue(field, target, buildClone(target, field, cloneValue, null));
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}
