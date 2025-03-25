package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

@OWLClass(iri = Vocabulary.CLASS_IRI_BASE + "GenericSubclass")
public class GenericSubclass extends AbstractGenericClass<OWLClassA> {
}
