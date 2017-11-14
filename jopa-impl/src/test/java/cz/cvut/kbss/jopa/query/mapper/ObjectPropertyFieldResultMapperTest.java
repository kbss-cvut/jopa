/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.ResultSet;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ObjectPropertyFieldResultMapperTest {

    @Mock
    private ResultSet resultSetMock;

    @Mock
    private UnitOfWork uowMock;

    private MetamodelMocks metamodelMocks;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    public void mapLoadsInstanceByIdentifierFromUnitOfWork() throws Exception {
        final FieldResultMapper mapper = new ObjectPropertyFieldResultMapper(
                metamodelMocks.forOwlClassD().owlClassAAtt());
        final String fieldName = metamodelMocks.forOwlClassD().owlClassAAtt().getName();
        final URI identifier = Generators.createIndividualIdentifier();
        when(resultSetMock.getObject(fieldName)).thenReturn(identifier);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        aInstance.setUri(identifier);
        when(uowMock.readObject(eq(OWLClassA.class), eq(identifier), any())).thenReturn(aInstance);

        final OWLClassD target = new OWLClassD();
        mapper.map(resultSetMock, target, uowMock);
        assertEquals(aInstance, target.getOwlClassA());
        verify(uowMock).readObject(OWLClassA.class, identifier, new EntityDescriptor());
    }

    @Test
    public void mapUsesValueDirectlyWhenFieldIsPlainIdentifier() throws Exception {
        final FieldResultMapper mapper = new ObjectPropertyFieldResultMapper(
                metamodelMocks.forOwlClassP().pUriAttribute());
        final String fieldName = metamodelMocks.forOwlClassP().pUriAttribute().getName();
        final URI value = Generators.createIndividualIdentifier();
        when(resultSetMock.getObject(fieldName)).thenReturn(value);
        final OWLClassP target = new OWLClassP();

        mapper.map(resultSetMock, target, uowMock);
        assertEquals(value, target.getIndividualUri());
        verify(uowMock, never()).readObject(any(), eq(value), any());
    }
}