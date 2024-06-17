/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class EntityResultMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWork uowMock;

    @Mock
    private IdentifiableEntityType<OWLClassA> etMock;

    private EntityResultMapper<OWLClassA> mapper;

    @BeforeEach
    void setUp() {
        this.mapper = new EntityResultMapper<>(etMock);
        when(etMock.getJavaType()).thenReturn(OWLClassA.class);
        when(uowMock.getLoadStateRegistry()).thenReturn(new LoadStateDescriptorRegistry(Object::toString));
    }

    @Test
    void mapCreatesNewInstanceOfTargetTypeAndRegistersItInUOW() {
        final OWLClassA clone = new OWLClassA();
        when(uowMock.registerExistingObject(any(), any(CloneRegistrationDescriptor.class))).thenReturn(clone);
        final OWLClassA result = mapper.map(resultRow, uowMock);
        assertNotNull(result);
        verify(uowMock).registerExistingObject(any(), any(CloneRegistrationDescriptor.class));
    }

    @Test
    void mapUsesFieldMappersToPopulateEntityFields() {
        final FieldSpecification fs = mock(FieldSpecification.class);
        when(fs.getDeclaringType()).thenReturn(etMock);
        final FieldResultMapper fOne = mock(FieldResultMapper.class);
        when(fOne.getFieldSpecification()).thenReturn(fs);
        final FieldResultMapper fTwo = mock(FieldResultMapper.class);
        when(fTwo.getFieldSpecification()).thenReturn(fs);
        mapper.addFieldMapper(fOne);
        mapper.addFieldMapper(fTwo);

        mapper.map(resultRow, uowMock);
        verify(fOne).map(eq(resultRow), any(), eq(uowMock));
        verify(fTwo).map(eq(resultRow), any(), eq(uowMock));
    }
}
