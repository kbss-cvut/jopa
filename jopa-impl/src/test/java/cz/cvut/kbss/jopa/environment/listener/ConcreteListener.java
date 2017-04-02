package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;

public class ConcreteListener {

    @PrePersist
    protected void prePersist(OWLClassR instance) {
        System.out.println("prePersist");
    }
}
