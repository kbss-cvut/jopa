package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectMultilingualString;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;

class MultilingualStringInstanceBuilder extends AbstractInstanceBuilder {

    MultilingualStringInstanceBuilder(CloneBuilderImpl builder, UnitOfWorkImpl uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, CloneConfiguration cloneConfiguration) {
        if (original == null) {
            return null;
        }
        assert original instanceof MultilingualString;
        MultilingualString orig = (MultilingualString) original;
        if (orig instanceof IndirectMultilingualString) {
            orig = ((IndirectMultilingualString) orig).unwrap();
        }
        return new IndirectMultilingualString(cloneOwner, field, uow, new MultilingualString(orig.getValue()));
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) {
        MultilingualString clone = (MultilingualString) cloneValue;
        if (clone instanceof IndirectMultilingualString) {
            clone = ((IndirectMultilingualString) clone).unwrap();
        }
        EntityPropertiesUtils
                .setFieldValue(field, target, clone != null ? new MultilingualString(clone.getValue()) : null);
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}
