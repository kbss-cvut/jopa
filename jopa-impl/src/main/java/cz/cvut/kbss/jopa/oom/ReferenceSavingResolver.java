package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

class ReferenceSavingResolver {

    private final ObjectOntologyMapperImpl mapper;

    ReferenceSavingResolver(ObjectOntologyMapperImpl mapper) {
        this.mapper = mapper;
    }

    /**
     * Checks whether an object property assertion (reference to the target instance) should be inserted into the storage.
     * <p>
     * A reference should be saved if:
     * <ul>
     * <li>The value is {@code null},</li>
     * <li>The value is a plain identifier,</li>
     * <li>The value is already managed,</li>
     * <li>The value is not managed, but exists in the storage.</li>
     * </ul>
     * <p>
     * Otherwise, the reference should not be saved and should be registered as pending.
     *
     * @param valueType Java type of the value
     * @param value     The value to save
     * @param context   Storage context
     * @return Whether to save the corresponding assertion or not
     */
    boolean shouldSaveReference(Class<?> valueType, Object value, URI context) {
        return value == null || IdentifierTransformer.isValidIdentifierType(valueType) || shouldSaveReferenceToItem(
                valueType, value, context);
    }

    /**
     * Same as {@link #shouldSaveReference(Class, Object, URI)}, but skips null-check and check whether the value is a plain identifier.
     * <p>
     * Used for collections.
     */
    boolean shouldSaveReferenceToItem(Class<?> valueType, Object value, URI context) {
        if (mapper.isManaged(value)) {
            return true;
        }
        final EntityType<?> et = mapper.getEntityType(valueType);
        assert et != null;
        final URI identifier = EntityPropertiesUtils.getPrimaryKey(value, et);
        return identifier != null && mapper.containsEntity(et.getJavaType(), identifier, new EntityDescriptor(context));
    }

    /**
     * Registers a pending assertion in the mapper.
     * <p>
     * Before commit, all pending assertions have to be resolved, otherwise the commit fails.
     *
     * @param subject   Subject of the assertion
     * @param assertion Assertion representing the property
     * @param object    Value of the assertion (object)
     * @param context   Context, into which the assertion should be saved
     */
    void registerPendingReference(NamedResource subject, Assertion assertion, Object object, URI context) {
        mapper.registerPendingAssertion(subject, assertion, object, context);
    }
}
