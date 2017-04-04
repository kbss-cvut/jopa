package cz.cvut.kbss.jopa.environment.listener;

import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.model.annotations.PrePersist;
import cz.cvut.kbss.jopa.model.annotations.PreUpdate;

public class MappedSuperclassListener {

    @PrePersist
    @PreUpdate
    void combinedListener(QMappedSuperclass instance) {
        System.out.println("combined prePresist and preUpdate listener");
    }
}
