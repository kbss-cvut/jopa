package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Individual;
import cz.cvut.kbss.jopa.vocabulary.OWL;

public enum ObjectOneOfEnum {

    @Individual(iri = OWL.DATATYPE_PROPERTY)
    DATATYPE_PROPERTY,
    @Individual(iri = OWL.ANNOTATION_PROPERTY)
    OBJECT_PROPERTY,
    @Individual(iri = OWL.ANNOTATION_PROPERTY)
    ANNOTATION_PROPERTY
}
