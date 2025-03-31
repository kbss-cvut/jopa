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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ManagedTypeChangeDetectorTest {

    @Mock
    private MetamodelProvider metamodelProvider;

    @InjectMocks
    private ManagedTypeChangeDetector sut;

    @ParameterizedTest
    @MethodSource("testParams")
    void hasChangesReturnsChangesWhenIdentifierDiffers(OWLClassA clone, OWLClassA original, Boolean expected) throws Exception {
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        when(metamodelProvider.getMetamodel()).thenReturn(metamodel);
        final IdentifiableEntityType<OWLClassA> et = mock(IdentifiableEntityType.class);
        when(metamodel.entity(OWLClassA.class)).thenReturn(et);
        final Identifier idAtt = mock(Identifier.class);
        when(et.getIdentifier()).thenReturn(idAtt);
        when(idAtt.getJavaField()).thenReturn(OWLClassA.class.getDeclaredField("uri"));

        assertEquals(expected, sut.hasChanges(clone, original));
    }

    static Stream<Arguments> testParams() {
        final OWLClassA aOne = Generators.generateOwlClassAInstance();
        final OWLClassA aTwo = Generators.generateOwlClassAInstance();
        final OWLClassA aNew = Generators.generateOwlClassAInstance();
        aNew.setUri(null);
        final URI oneId = Generators.createIndividualIdentifier();
        return Stream.of(
                Arguments.of(aOne, aTwo, true),
                Arguments.of(aOne, aOne, false),
                Arguments.of(aOne, aNew, true),
                Arguments.of(aNew, aOne, true),
                Arguments.of(aNew, aNew, false)
        );
    }
}
