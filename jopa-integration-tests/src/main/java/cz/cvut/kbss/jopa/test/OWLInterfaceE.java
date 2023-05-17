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
    @OWLDataProperty(iri = Vocabulary.p_m_enumeratedOrdinalColor)
    Color getOrdinalEnumAttribute();

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.p_m_simpleList)
    List<URI> getSimpleList();

}
