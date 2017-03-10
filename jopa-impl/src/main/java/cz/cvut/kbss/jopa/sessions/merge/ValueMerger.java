package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

public interface ValueMerger {

    /**
     * Merges new value of the specified field into the target object.
     *
     * @param att                 The attribute to merge
     * @param target              Target of the merge
     * @param originalValue       Original value of the field
     * @param mergedValue         The value to merge into the field
     * @param attributeDescriptor Specifies context of the merged attribute
     */
    void mergeValue(FieldSpecification<?, ?> att, Object target, Object originalValue, Object mergedValue,
                    Descriptor attributeDescriptor);
}
