package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_PARENT_B)
public interface OWLParentB {

      String getStringAttribute();

      void setStringAttribute(String stringAttribute);
}
