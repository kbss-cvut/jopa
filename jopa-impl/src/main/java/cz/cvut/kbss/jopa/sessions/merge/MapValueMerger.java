package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Map;

public class MapValueMerger implements ValueMerger {


    @Override
    public void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                           Descriptor attributeDescriptor) {
        final Map<?, ?> mergedMap = (Map<?, ?>) mergedValue;
        // This is a simplified version which will work only for the @Properties attributes
        // Bus since JOPA does not currently support any other use of Maps, it should be ok

        if (mergedMap == null) {
            EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, null);
            return;
        }

        final Map<Object, Object> newMap = CollectionFactory.createDefaultMap();
        newMap.putAll(mergedMap);
        EntityPropertiesUtils.setFieldValue(att.getJavaField(), target, newMap);
    }
}
