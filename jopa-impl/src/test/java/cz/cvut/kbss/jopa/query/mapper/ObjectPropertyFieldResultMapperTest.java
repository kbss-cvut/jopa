/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ObjectPropertyFieldResultMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWork uowMock;

    private MetamodelMocks metamodelMocks;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    void mapLoadsInstanceByIdentifierFromUnitOfWork() throws Exception {
        final FieldResultMapper mapper = new ObjectPropertyFieldResultMapper(
                metamodelMocks.forOwlClassD().owlClassAAtt());
        final String fieldName = metamodelMocks.forOwlClassD().owlClassAAtt().getName();
        final URI identifier = Generators.createIndividualIdentifier();
        when(resultRow.isBound(fieldName)).thenReturn(true);
        when(resultRow.getObject(fieldName)).thenReturn(identifier);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        aInstance.setUri(identifier);
        when(uowMock.readObjectWithoutRegistration(eq(OWLClassA.class), eq(identifier), any())).thenReturn(aInstance);

        final OWLClassD target = new OWLClassD();
        mapper.map(resultRow, target, uowMock);
        assertEquals(aInstance, target.getOwlClassA());
        verify(uowMock).readObjectWithoutRegistration(OWLClassA.class, identifier, new EntityDescriptor());
    }

    @Test
    void mapUsesValueDirectlyWhenFieldIsPlainIdentifier() throws Exception {
        final FieldResultMapper mapper = new ObjectPropertyFieldResultMapper(
                metamodelMocks.forOwlClassP().pUriAttribute());
        final String fieldName = metamodelMocks.forOwlClassP().pUriAttribute().getName();
        final URI value = Generators.createIndividualIdentifier();
        when(resultRow.isBound(fieldName)).thenReturn(true);
        when(resultRow.getObject(fieldName)).thenReturn(value);
        final OWLClassP target = new OWLClassP();

        mapper.map(resultRow, target, uowMock);
        assertEquals(value, target.getIndividualUri());
        verify(uowMock, never()).readObject(any(), eq(value), any());
    }
}
