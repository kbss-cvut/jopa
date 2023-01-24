package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.net.URI;

@OWLClass(iri = Vocabulary.c_Phone)
public class Phone {

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_p_phoneNumber)
    private String number;
}
