package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.model.annotations.PrePersist;

public class AnotherListener {

    @PrePersist
    public void prePersist(Object instance) {
        System.out.println("prePersist");
    }
}
