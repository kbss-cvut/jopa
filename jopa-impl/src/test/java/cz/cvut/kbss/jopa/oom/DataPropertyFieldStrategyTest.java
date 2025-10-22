/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class DataPropertyFieldStrategyTest {

    @Mock
    private EntityMappingHelper mapperMock;


    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        descriptor = spy(descriptor);
    }

    @Test
    void getLanguagesUsesAttributeConfiguredLanguageWhenDescriptorDoesNotSpecifyAny() {
        final DataPropertyFieldStrategy<?, ?> sut = (DataPropertyFieldStrategy<?, ?>) FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                        metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertTrue(metamodelMocks.forOwlClassA().stringAttribute().hasLanguage());
        assertEquals(Generators.LANG, sut.getLanguage());
    }

    @Test
    void getLanguageRetrievesAttributeLanguageFromEntityDescriptor() {
        final String lang = "cs";
        descriptor.setAttributeLanguage(metamodelMocks.forOwlClassA().stringAttribute(), lang);
        final DataPropertyFieldStrategy<?, ?> sut = (DataPropertyFieldStrategy<?, ?>) FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassA().entityType(),
                        metamodelMocks.forOwlClassA().stringAttribute(), descriptor, mapperMock);
        assertEquals(lang, sut.getLanguage());
    }

    @Test
    void getLanguageReturnsNullForSimpleLiteralAttribute() {
        final DataPropertyFieldStrategy<?, ?> sut = (DataPropertyFieldStrategy<?, ?>) FieldStrategy
                .createFieldStrategy(metamodelMocks.forOwlClassM().entityType(),
                        metamodelMocks.forOwlClassM().simpleLiteralAttribute(), descriptor, mapperMock);
        assertNull(sut.getLanguage());
    }
}
