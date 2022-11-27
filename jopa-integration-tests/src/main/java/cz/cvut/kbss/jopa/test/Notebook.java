package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;

@OWLClass(iri = Vocabulary.NOTEBOOK)
public interface Notebook {



    public abstract String getStringAttribute();

    public abstract void setStringAttribute(String stringAttribute);
}
