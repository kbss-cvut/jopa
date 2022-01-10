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
package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.Objects;

/**
 * Validates modification operations on attributes.
 */
public class AttributeModificationValidator {

    private AttributeModificationValidator() {
        throw new AssertionError();
    }

    /**
     * Checks whether the specified field can be modified.
     *
     * @param fieldSpec Specification of mapped field to verify
     * @throws AttributeModificationForbiddenException If the specified field cannot be modified
     */
    public static void verifyCanModify(FieldSpecification<?, ?> fieldSpec) {
        Objects.requireNonNull(fieldSpec);
        if (fieldSpec.isInferred()) {
            throw new InferredAttributeModifiedException(
                    "Field " + fieldSpec + " may contain inferences and cannot be modified.");
        }
        if (fieldSpec instanceof AbstractAttribute && ((AbstractAttribute<?, ?>) fieldSpec).isLexicalForm()) {
            throw new AttributeModificationForbiddenException("Field " + fieldSpec +
                    " is configured to contain lexical form of literals and cannot be modified.");
        }
    }
}
