package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.util.Set;
@OWLClass(iri = Vocabulary.C_OWL_CLASS_PARENT_A)
public interface OWLParentA {

    Set<String> getPluralAnnotationProperty();

    void setPluralAnnotationProperty(Set<String> pluralAnnotationProperty);
}
