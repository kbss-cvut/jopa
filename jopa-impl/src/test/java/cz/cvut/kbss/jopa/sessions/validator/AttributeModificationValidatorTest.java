/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AttributeModificationValidatorTest {

    @Test
    void verifyCanModifyAllowsModificationOfInferredAttributes() {
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        when(fs.isInferred()).thenReturn(true);
        assertDoesNotThrow(() -> AttributeModificationValidator.verifyCanModify(fs));
    }

    @Test
    void verifyCanModifyThrowsAttributeModificationForbiddenWhenLexicalFormAttributeValueIsModified() {
        final AbstractAttribute<?, ?> att = mock(AbstractAttribute.class);
        when(att.isLexicalForm()).thenReturn(true);
        assertThrows(AttributeModificationForbiddenException.class,
                () -> AttributeModificationValidator.verifyCanModify(att));
    }

    @Test
    void verifyCanModifyAllowsModificationToRegularFields() {
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        assertDoesNotThrow(() -> AttributeModificationValidator.verifyCanModify(fs));
    }
}
