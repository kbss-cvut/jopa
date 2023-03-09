package cz.cvut.kbss.jopa.test;


import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.Transient;

import java.net.URI;

@OWLClass(iri = Vocabulary.C_OwlClassWithUnProperties)
public class OWLClassWithUnProperties implements OWLInterfaceAnMethods {
    @Id
    private URI id;

    @Transient
    private boolean used;
    private String name;

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }
    public boolean getUsed() {
        return  used;
    }
    public boolean isUsed(){
        return used;
    }

}
