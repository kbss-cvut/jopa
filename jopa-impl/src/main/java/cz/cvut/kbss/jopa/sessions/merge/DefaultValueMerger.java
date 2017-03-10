package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class DefaultValueMerger implements ValueMerger {

    @Override
    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                           Descriptor attributeDescriptor) {
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, mergedValue);
    }
}
