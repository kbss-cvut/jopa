/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
