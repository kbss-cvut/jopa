/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.util.Map;
import java.util.Objects;

public abstract class IntegrityConstraintsValidator {

    private static IntegrityConstraintsValidator generalValidator = initGeneralValidator();

    private static IntegrityConstraintsValidator initGeneralValidator() {
        final GeneralIntegrityConstraintsValidator validator = new GeneralIntegrityConstraintsValidator();
        validator.addValidator(new CardinalityConstraintsValidator());
        return validator;
    }


    public static IntegrityConstraintsValidator getValidator() {
        return generalValidator;
    }

    /**
     * Validates integrity constraints of all attributes of the specified instance.
     *
     * @param instance The instance to validate
     * @param et       EntityType of the instance
     * @param skipLazy Whether to skip validation of lazily loaded attributes
     * @param <T>      Entity class type
     */
    public <T> void validate(T instance, EntityType<T> et, boolean skipLazy) {
        Objects.requireNonNull(instance, ErrorUtils.constructNPXMessage("instance"));
        Objects.requireNonNull(et, ErrorUtils.constructNPXMessage("et"));

        final Object id = EntityPropertiesUtils.getPrimaryKey(instance, et);
        for (Attribute<T, ?> att : et.getDeclaredAttributes()) {
            if (skipLazy && att.getFetchType() == FetchType.LAZY) {
                continue;
            }
            final Object value = EntityPropertiesUtils.getAttributeValue(att, instance);
            validate(id, att, value);
        }
    }

    /**
     * Validates integrity constraints for changes in the specified change set.
     *
     * @param changeSet The change set to validate
     */
    public void validate(ObjectChangeSet changeSet, Metamodel metamodel) {
        Objects.requireNonNull(changeSet, ErrorUtils.constructNPXMessage("changeSet"));
        Objects.requireNonNull(metamodel, ErrorUtils.constructNPXMessage("metamodel"));

        final EntityType<?> et = metamodel.entity(changeSet.getObjectClass());
        final Object id = EntityPropertiesUtils.getPrimaryKey(changeSet.getCloneObject(), et);
        for (Map.Entry<String, ChangeRecord> entry : changeSet.getChanges().entrySet()) {
            final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(entry.getKey());
            validate(id, fieldSpec, entry.getValue().getNewValue());
        }
    }

    /**
     * Validates whether the specified value conforms to the attribute integrity constraints.
     *
     * @param identifier     Instance identifier
     * @param attribute      Attribute metadata with integrity constraints
     * @param attributeValue Value to be validated
     */
    public abstract void validate(Object identifier, FieldSpecification<?, ?> attribute, Object attributeValue);
}
