package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.model.annotations.PostPersist;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;

public class ParentListener {

    @PrePersist
    public void prePersist() {
        System.out.println("PrePersist");
    }

    @PostPersist
    private void postPersist(Object instance) {
        System.out.println("Private postPersist");
    }
}
