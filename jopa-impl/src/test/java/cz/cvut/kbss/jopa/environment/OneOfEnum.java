package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Individual;
import cz.cvut.kbss.jopa.model.annotations.ObjectOneOf;
import cz.cvut.kbss.jopa.vocabulary.OWL;

@ObjectOneOf
public enum OneOfEnum {

    @Individual(iri = OWL.OBJECT_PROPERTY)
    OBJECT_PROPERTY,
    @Individual(iri = OWL.DATATYPE_PROPERTY)
    DATATYPE_PROPERTY,
    @Individual(iri = OWL.ANNOTATION_PROPERTY)
    ANNOTATION_PROPERTY
}
