/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.List;
import java.util.Set;

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
     * @param value     The value to save
     * @param contexts   Storage contexts
     * @return Whether to save the corresponding assertion or not
     */
    boolean shouldSaveReference(Object value, Set<URI> contexts) {
        return value == null || IdentifierTransformer.isValidIdentifierType(value.getClass()) || shouldSaveReferenceToItem(value, contexts);
    }

    /**
     * Same as {@link #shouldSaveReference(Object, Set)}, but skips null-check and check whether the value is a plain identifier.
     * <p>
     * Used for collections.
     */
    boolean shouldSaveReferenceToItem(Object value, Set<URI> contexts) {
        if (mapper.isManaged(value) || value.getClass().isEnum()) {
            return true;
        }
        final EntityType<?> et = mapper.getEntityType(value.getClass());
        assert et != null;
        final URI identifier = EntityPropertiesUtils.getIdentifier(value, et);
        return identifier != null && mapper.containsEntity(et.getJavaType(), identifier, new EntityDescriptor(contexts));
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

    /**
     * Registers a pending reference to a list (simple or referenced).
     * <p>
     * This means that at least one item in the list has not yet been persisted. Since the list is linked, all items
     * must be persisted before the list itself is inserted into the storage.
     *
     * @param item           Pending list item
     * @param listDescriptor Descriptor of the list
     * @param values         The whole list
     */
    void registerPendingReference(Object item, ListValueDescriptor listDescriptor, List<?> values) {
        mapper.registerPendingListReference(item, listDescriptor, values);
    }
}
