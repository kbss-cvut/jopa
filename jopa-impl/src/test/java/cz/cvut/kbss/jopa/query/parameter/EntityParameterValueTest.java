/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

class EntityParameterValueTest {

    @Mock
    private MetamodelProvider metamodelProvider;

    @Mock
    private Metamodel metamodel;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        final MetamodelMocks metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        when(metamodelProvider.getMetamodel()).thenReturn(metamodel);
    }

    @Test
    void getQueryStringReturnsEntityIdentifierAsQueryResource() {
        when(metamodelProvider.isEntityType(OWLClassA.class)).thenReturn(true);
        final OWLClassA value = Generators.generateOwlClassAInstance();
        final EntityParameterValue sut = new EntityParameterValue(value, metamodelProvider);
        assertEquals("<" + value.getUri() + ">", sut.getQueryString());
    }

    @Test
    void getQueryStringReturnsStringEntityIdentifierAsQueryResource() {
        when(metamodelProvider.isEntityType(OWLClassM.class)).thenReturn(true);
        final OWLClassM value = new OWLClassM();
        value.setKey("http://testM");
        final EntityParameterValue sut = new EntityParameterValue(value, metamodelProvider);
        assertEquals("<" + value.getKey() + ">", sut.getQueryString());
    }
}
