package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.util.Set;
@OWLClass(iri = Vocabulary.TABLET)
public interface Tablet {

    Set<String> getPluralAnnotationProperty();

    void setPluralAnnotationProperty(Set<String> pluralAnnotationProperty);
}
