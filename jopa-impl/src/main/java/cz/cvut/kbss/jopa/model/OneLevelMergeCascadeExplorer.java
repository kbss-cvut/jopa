package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;

import java.util.Arrays;
import java.util.List;

/**
 * Merge cascade resolver.
 * <p>
 * Cascades the merge operation to attributes which support merge cascading.
 */
class OneLevelMergeCascadeExplorer {

    void start(final AbstractEntityManager pc, final Object merged, final Object toMerge) {

        final EntityType<?> a = pc.getMetamodel().entity(toMerge.getClass());
        for (final Attribute<?, ?> at : a.getAttributes()) {

            final List<CascadeType> cTypes = Arrays.asList(at.getCascadeTypes());

            try {
                if (cTypes.contains(CascadeType.ALL) || cTypes.contains(CascadeType.MERGE)) {
                    exploreCascaded(at, merged, toMerge);
                }
            } catch (Exception e) {
                throw new OWLPersistenceException(e);
            }
        }
    }

    void exploreCascaded(final Attribute<?, ?> at, final Object merged, final Object toMerge)
            throws IllegalAccessException {
        // empty body
    }
}
