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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Set;


@OWLClass(iri = Vocabulary.C_OWL_INTERFACE_E)
public interface OWLInterfaceE {
    enum Color {
        BLUE, BLACK, RED, GREEN
    }
    @ParticipationConstraints(nonEmpty = true)
    @OWLObjectProperty(iri = Vocabulary.p_m_data, cascade = CascadeType.ALL)
    OWLClassWithUnProperties getData();

    @ParticipationConstraints(value = @ParticipationConstraint(owlObjectIRI = Vocabulary.C_OwlClassWithUnProperties, max = 2))
    @OWLObjectProperty(iri = Vocabulary.p_m_multiple_data, cascade = CascadeType.PERSIST)
    Set<OWLClassWithUnProperties> getDataList();

    @Convert(converter = ZoneOffsetConverter.class)
    @OWLDataProperty(iri = Vocabulary.p_m_withConverter, simpleLiteral = true)
    ZoneOffset getWithConverter();

    @Enumerated(EnumType.ORDINAL)
    @OWLDataProperty(iri = Vocabulary.p_e_enumeratedOrdinalColor)
    Color getOrdinalEnumAttribute();

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.p_e_simpleList)
    List<URI> getSimpleList();

}
