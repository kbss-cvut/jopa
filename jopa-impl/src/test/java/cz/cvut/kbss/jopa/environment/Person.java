package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.annotations.*;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.c_Person)
public class Person {

    @Id
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.p_p_username)
    private String username;

    @OWLDataProperty(iri = Vocabulary.p_p_gender)
    private String gender;

    @OWLDataProperty(iri = Vocabulary.p_p_age)
    private Integer age;

    @OWLObjectProperty(iri = Vocabulary.p_p_hasPhone)
    private Phone phone;

    @Types
    private Set<String> types;
}
