package cz.cvut.kbss.jopa.sessions.validator;

import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class AttributeModificationValidatorTest {

    @Test
    void verifyCanModifyThrowsInferredAttributeModifiedExceptionWhenInferredAttributeIsModified() {
        final FieldSpecification<?, ?> fs = mock(FieldSpecification.class);
        when(fs.isInferred()).thenReturn(true);
        assertThrows(InferredAttributeModifiedException.class,
                () -> AttributeModificationValidator.verifyCanModify(fs));
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