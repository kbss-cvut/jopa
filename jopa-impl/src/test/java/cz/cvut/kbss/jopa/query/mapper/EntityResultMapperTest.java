/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
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
    private UnitOfWorkImpl uowMock;

    @Mock
    private IdentifiableEntityType<OWLClassA> etMock;

    private EntityResultMapper<OWLClassA> mapper;

    @BeforeEach
    void setUp() {
        this.mapper = new EntityResultMapper<>(etMock);
        when(etMock.getJavaType()).thenReturn(OWLClassA.class);
    }

    @Test
    void mapCreatesNewInstanceOfTargetTypeAndRegistersItInUOW() {
        final OWLClassA clone = new OWLClassA();
        when(uowMock.registerExistingObject(any(), any(), any())).thenReturn(clone);
        final OWLClassA result = mapper.map(resultRow, uowMock);
        assertNotNull(result);
        verify(uowMock).registerExistingObject(any(), any(), any());
    }

    @Test
    void mapUsesFieldMappersToPopulateEntityFields() {
        final FieldResultMapper fOne = mock(FieldResultMapper.class);
        final FieldResultMapper fTwo = mock(FieldResultMapper.class);
        mapper.addFieldMapper(fOne);
        mapper.addFieldMapper(fTwo);

        mapper.map(resultRow, uowMock);
        verify(fOne).map(eq(resultRow), any(), eq(uowMock));
        verify(fTwo).map(eq(resultRow), any(), eq(uowMock));
    }
}
