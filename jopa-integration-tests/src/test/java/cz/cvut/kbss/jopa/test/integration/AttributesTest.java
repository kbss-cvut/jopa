package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

class AttributesTest extends IntegrationTestBase {

    @Test
    void updatingLexicalFormAttributeThrowsAttributeModificationForbiddenException() throws Exception {
        final String key = Generators.generateUri().toString();
        final Axiom<NamedResource> classAssertion = new AxiomImpl<>(NamedResource.create(key),
                Assertion.createClassAssertion(false), new Value<>(NamedResource.create(
                Vocabulary.C_OWL_CLASS_M)));
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(Collections.singletonList(classAssertion));
        em.getTransaction().begin();
        final OWLClassM m = em.find(OWLClassM.class, key);
        assertNotNull(m);
        assertThrows(AttributeModificationForbiddenException.class, () -> m.setLexicalForm("test"));
    }
}
